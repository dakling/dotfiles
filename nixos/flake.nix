{
  description = "NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Emacs overlay for Doom Emacs
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Qtile extra widgets and utilities
    # qtile-extras = {
    #   url = "github:elParaguayo/qtile-extras/main";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    sops-nix,
    emacs-overlay,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      config.permittedInsecurePackages = [
        "qtwebkit-5.212.0-alpha4"
      ];
      overlays = [
        emacs-overlay.overlays.default
      ];
    };
    pkgs-unstable = import nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
      config.permittedInsecurePackages = [
        "qtwebkit-5.212.0-alpha4"
      ];
    };
  in {
    nixosConfigurations = {
      nixos-laptop = nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = {
          inherit inputs pkgs-unstable;
        };
        modules = [
          ./configuration.nix
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.helario = import ./home.nix;
            home-manager.extraSpecialArgs = {inherit inputs pkgs-unstable;};
          }
        ];
      };
    };
  };
}
