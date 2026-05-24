# Hardware configuration
# Intel graphics, audio, bluetooth, touchpad, printers

{pkgs, ...}: {
  # Intel Graphics
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver # VA-API
      intel-vaapi-driver
      libvdpau-va-gl
      vulkan-loader
      vulkan-tools
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      intel-vaapi-driver
      vulkan-loader
    ];
  };

  # OpenGL
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "iHD";
    MOZ_DISABLE_RIO = "1";
  };

  # Audio - PipeWire
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
        Experimental = true;
      };
    };
  };
  services.blueman.enable = true;

  # Touchpad - libinput configuration
  services.libinput = {
    enable = true;
    touchpad = {
      tapping = true;
      tappingButtonMap = "lrm"; # 1, 2, 3 finger tap
      naturalScrolling = true;
      disableWhileTyping = true;
      clickMethod = "clickfinger";
      scrollMethod = "twofinger";
      middleEmulation = false;
    };
    mouse = {
      naturalScrolling = false;
      middleEmulation = false;
    };
  };

  # Printing - CUPS
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      # hplip for HP printers if needed
      hplip
    ];
    browsing = true;
    defaultShared = true;
    listenAddresses = ["*:631"];
    allowFrom = ["all"];
    extraConf = ''
      DefaultEncryption Never
    '';
  };

  # Scanning
  hardware.sane = {
    enable = true;
    netConf = ''
      # Network scanner configuration
    '';
  };

  # Keyboard backlight for TUXEDO laptops
  # Note: tuxedo-keyboard may need special handling
  # Check if tuxedo-nix overlay is needed
  boot.extraModulePackages = with pkgs; [
    # tuxedo-keyboard may need to be added from tuxedo-nix or packaged
  ];

  # Firmware updates
  services.fwupd.enable = true;



  # PCMCIA (for older hardware)
  services.pcscd.enable = true;

  # Hardware packages
  environment.systemPackages = with pkgs; [
    # Hardware tools
    acpi
    powertop
    cpupower-gui
    
    # Audio tools
    pavucontrol
    pamixer
    wireplumber

    # Bluetooth
    bluez
    bluez-tools
    obexftp

    # Scanning
    simple-scan
    xsane
  ];
}
