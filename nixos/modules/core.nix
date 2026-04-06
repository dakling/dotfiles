# Core system configuration
# Boot, users, locale, timezone, filesystems

{pkgs, ...}: {
  # Bootloader
  boot = {
    loader = {
      grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
        useOSProber = true; # Detect other OS installations
      };
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot";
    };

    # Kernel
    kernelPackages = pkgs.linuxPackages_latest;

    # Kernel parameters for Intel laptop
    kernelParams = [
      "i915.force_probe=4a78" # Tiger Lake
      "i915.enable_guc=2"
      "i915.enable_fbc=1"
    ];
  };

  # Timezone
  time.timeZone = "Europe/Berlin";

  # Locale
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "de_DE.UTF-8";
      LC_IDENTIFICATION = "de_DE.UTF-8";
      LC_MEASUREMENT = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
      LC_NAME = "de_DE.UTF-8";
      LC_NUMERIC = "de_DE.UTF-8";
      LC_PAPER = "de_DE.UTF-8";
      LC_TELEPHONE = "de_DE.UTF-8";
      LC_TIME = "de_DE.UTF-8";
    };
  };

  # Console keymap
  console.keyMap = "de-latin1-nodeadkeys";

  # User account
  users.users.klingenberg = {
    isNormalUser = true;
    description = "klingenberg";
    extraGroups = ["networkmanager" "wheel" "docker" "audio" "video" "lp" "scanner"];
    shell = pkgs.bash;
    # Initial password - should be changed after first login
    initialPassword = "changeme";
  };



  # Networking
  networking = {
    hostName = "nixos-laptop";

    # Firewall
    firewall = {
      enable = true;
      allowedTCPPorts = [
        # Syncthing
        22000
        # CUPS
        631
      ];
      allowedUDPPorts = [
        # Syncthing discovery
        21027
      ];
    };

    # NetworkManager is configured in services.nix
    networkmanager.enable = true;
  };

  # Sops-nix configuration for secrets
  sops = {
    defaultSopsFile = ./secrets/secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/home/klingenberg/.config/sops/age/keys.txt";
  };

  # Security
  security = {
    # Polkit for GUI authentication
    polkit.enable = true;

    # Real-time scheduling for audio
    rtkit.enable = true;
  };

  # Filesystems support
  services = {
    # Auto-mounting
    udisks2.enable = true;

    # Trim SSD
    fstrim.enable = true;
  };

  # Power management
  powerManagement = {
    enable = true;
    cpuFreqGovernor = "powersave";
  };

  # Laptop power management
  services.upower = {
    enable = true;
    percentageLow = 15;
    percentageCritical = 5;
    percentageAction = 3;
    criticalPowerAction = "Hibernate";
  };
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
    };
  };
}