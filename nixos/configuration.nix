# Main NixOS configuration
# Imports all modules and defines system-wide settings

{pkgs, pkgs-unstable, inputs, ...}: {
  imports = [
    ./hardware-configuration.nix
    ./modules/core.nix
    ./modules/hardware.nix
    ./modules/desktop.nix
    ./modules/services.nix
    ./modules/xinit.nix
  ];

  # System packages that don't fit in other modules
  environment.systemPackages = with pkgs; [
    # Basic utilities
    wget
    curl
    git
    htop
    btop
    tree
    ripgrep
    fd
    bat
    eza
    fzf
    jq
    yq
    neovim
    tmux
    kitty

    # Archives
    zip
    unzip
    p7zip

    # System tools
    parted
    gparted
    ntfs3g
    exfat
    udiskie
    udisks2
    pasystray

    # Man pages
    man-pages
    man-db

    firefox
    flatpak

    pkgs-unstable.opencode
  ];

  # Nix settings
  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    auto-optimise-store = true;
    trusted-users = ["helario"];
  };

  # Garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # This value determines the NixOS release from which certain defaults
  # are set (e.g., collection of enabled packages).
  system.stateVersion = "24.11";
}
