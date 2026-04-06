# System services
# NetworkManager, Docker, CUPS, Syncthing, custom services

{pkgs, pkgs-unstable, ...}: {
  # NetworkManager
  networking.networkmanager = {
    enable = true;
    dns = "systemd-resolved";
    wifi.powersave = true;
  };

  # DNS resolver
  services.resolved = {
    enable = true;
    dnssec = "allow-downgrade";
    llmnr = "true";
    extraConfig = ''
      DNS=1.1.1.1 1.0.0.1 2606:4700:4700::1111 2606:4700:4700::1001
      FallbackDNS=9.9.9.9 2620:fe::fe
    '';
  };

  # SSH
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  # Docker
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
    };
    # Use overlay2 driver
    storageDriver = "overlay2";
    daemon.settings = {
      log-opts = {
        max-size = "10m";
        max-file = "3";
      };
    };
  };



  # Avahi for network printer discovery
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
    publish = {
      enable = true;
      userServices = true;
    };
  };

  # Syncthing - per-user service configuration in home.nix
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    configDir = "/home/klingenberg/.config/syncthing";
    dataDir = "/home/klingenberg/Sync";
    user = "klingenberg";
    group = "users";
    guiAddress = "127.0.0.1:8384";
  };

  # Cronie - system cron
  services.cron = {
    enable = true;
    systemCronJobs = [
      # Add system cron jobs here
      # Example: "0 0 * * * root /path/to/script"
    ];
  };

  # System timers as alternatives to cron
  systemd.timers = {
    # Example: Weekly backup timer
    # backup-timer = {
    #   wantedBy = [ "timers.target" ];
    #   timerConfig = {
    #     OnCalendar = "weekly";
    #     Persistent = true;
    #   };
    # };
  };

  # LiteLLM Gateway custom service
  systemd.services.litellm-gateway = {
    description = "LiteLLM Gateway";
    wantedBy = ["multi-user.target"];
    after = ["network-online.target"];
    wants = ["network-online.target"];
    serviceConfig = {
      Type = "simple";
      User = "klingenberg";
      Group = "users";
      WorkingDirectory = "/home/klingenberg/.local/share/litellm";
      ExecStart = "${pkgs.litellm}/bin/litellm --config /home/klingenberg/.config/litellm/config.yaml";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = "PATH=${pkgs.litellm}/bin";
    };
    path = [pkgs.litellm];
  };

  # Timesyncd (NTP)
  services.timesyncd = {
    enable = true;
    servers = [
      "0.de.pool.ntp.org"
      "1.de.pool.ntp.org"
      "2.de.pool.ntp.org"
      "3.de.pool.ntp.org"
    ];
  };

  # Geoclue for location services
  services.geoclue2 = {
    enable = true;
    enableDemoAgent = true;
  };

  # GVFS for file manager virtual filesystems
  services.gvfs.enable = true;

  # Tumbler for thumbnail generation
  services.tumbler.enable = true;

  # AppArmor for additional security (optional)
  # security.apparmor.enable = true;

  # ClamAV antivirus (optional)
  # services.clamav = {
  #   daemon.enable = true;
  #   updater.enable = true;
  # };

  # Logind configuration
  services.logind = {
    lidSwitch = "suspend";
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "suspend";
    extraConfig = ''
      HandlePowerKey=suspend
      HandleSuspendKey=suspend
      HandleHibernateKey=hibernate
      IdleAction=suspend
      IdleActionSec=30min
    '';
  };



  # System packages for services
  environment.systemPackages = with pkgs; [
    # Network tools
    networkmanager
    networkmanagerapplet
    wirelesstools
    iw

    # Docker tools
    docker
    docker-compose
    lazydocker

    # Syncthing
    syncthing
    syncthingtray

    # LiteLLM
    litellm

    # System monitoring
    htop
    btop
    glances
    iotop
    nethogs

    # Service management
    sysstat
    lm_sensors
  ];
}