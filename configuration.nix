{ config, pkgs, lib, ...}:

{

  imports = [
    ./hardware-configuration.nix
    ./local.nix
  ];

  nixpkgs = rec {
    crossSystem = (import <nixpkgs> {}).pkgsCross.aarch64-multiplatform.stdenv.targetPlatform;
    localSystem = crossSystem;
  };

  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 4;
  # boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_rpi4;
  # boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.kernelParams = ["cma=64M"];

  # fileSystems = {

  # "/" = {
  # device = "/dev/disk/by-label/NIXOS_SD";
  # fsType = "ext4";
  # };
  # };

  swapDevices = [{device = "/swapfile"; size = 1024;}];

  networking.hostName = "klingenberg-pi";

  time.timeZone = "Europe/Berlin";

  users.users.klingenberg = {
    isNormalUser = true;
    home = "/home/klingenberg";
    description = "klingenberg";
    extraGroups = ["wheel" "networkmanager" ];
  };

  hardware.enableRedistributableFirmware = true;

  services.xserver.enable = true;

  services.xserver.displayManager.defaultSession = "none+xsession";

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.user = "klingenberg";
  services.xserver.videoDrivers = [ "fbdev" ];
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "ctrl:nocaps";
  console.useXkbConfig = true;

  services.xserver.windowManager.session = lib.singleton {
    name = "xsession";
    start = pkgs.writeScript "xsession" ''
        #!${pkgs.runtimeShell}
        if test -f $HOME/.xsession; then
          exec ${pkgs.runtimeShell} -c $HOME/.xsession
        else
          echo "No xsession script found"
          exit 1
        fi
      '';
  };

  xsession.enable = true;
  xsession.windowManager.command = "emacs";

  nixpkgs.overlays = [
    (import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz))
  ];

  environment.systemPackages = with pkgs; [
    emacsUnstable
    # exwm
    git
    wget
    ripgrep
    coreutils
    fd
    clang
    firefox
    python
    # nixfmt
    # sbcl
    # quicklisp
    # nextcloud stuff
    # doom-doctor output stuff
  ];

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
  services.network-manager-applet.enable = true;

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.hicolor_icon_theme;
      name = "hicolor";
    };
  };
  # And make QT look the same
  qt = {
    useGtkTheme = true;
    enable = true;
  };

  services.dunst.enable = true;

}
