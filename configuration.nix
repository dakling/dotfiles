{ config, pkgs, lib, ...}:

{

  imports = [
    ./hardware-configuration.nix
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

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.desktopManager.xfce.enable = true;
  services.xserver.windowManager.exwm.enable = true;
  services.xserver.windowManager.exwm.enableDefaultConfig = false;
  services.xserver.videoDrivers = [ "fbdev" ];
  services.xserver.layout = "de";
  console.useXkbConfig = true;

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
  ];

}
