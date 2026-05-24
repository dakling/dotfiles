# System services
# NetworkManager, Docker, CUPS, Syncthing, custom services

{pkgs, ...}: {
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
  };

  # SSH
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
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
  };



  # System packages for services
  environment.systemPackages = with pkgs; [
    # Network tools
    networkmanager
    networkmanagerapplet
    wirelesstools
    iw

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
