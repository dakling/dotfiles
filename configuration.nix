{ config, pkgs, lib, ...}:

{

  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/local.nix
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

  # services.xserver.displayManager.defaultSession = "none+xsession";

  services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.displayManager.autoLogin.enable = true;
  # services.xserver.displayManager.autoLogin.user = "klingenberg";
  services.xserver.desktopManager.xfce.enable = true;
  services.xserver.windowManager = {
    session = lib.singleton {
      name = "exwm";
      start = ''
          eval `dbus-launch --exit-with-session ${pkgs.emacsUnstable}/bin/emacs`
        '';
    };
  };
  # windowManager = let
  #   emacs = pkgs.emacsUnstable.override {
  #     withXwidgets = true;
  #     withGTK3 = true;
  #   };
  # in {
  #       session = lib.singleton {
  #         name = "exwm";
  #         start = ''
  #           eval `dbus-launch --exit-with-session ${emacs}/bin/emacs`
  #         '';
  #       };
  #     };
  services.xserver.videoDrivers = [ "fbdev" ];
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "ctrl:nocaps";
  console.useXkbConfig = true;

  #xsession.enable = true;
  #xsession.windowManager.command = "emacs";

  nixpkgs.overlays = [
    (import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz))
  ];

  environment.systemPackages = with pkgs; [
    emacsUnstable
    git
    wget
    ripgrep
    coreutils
    fd
    clang
    firefox
    sqlite
    #   python
    #    nixfmt
    # sbcl
    # quicklisp
    # nextcloud stuff
    # doom-doctor output stuff
  ];

}
