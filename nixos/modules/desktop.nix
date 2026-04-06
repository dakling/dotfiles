# Desktop environment configuration
# XServer, Qtile, XFCE, fonts, themes

{pkgs, inputs, ...}: {
  # XServer
  services.xserver = {
    enable = true;

    # Keyboard configuration
    xkb = {
      layout = "de";
      variant = "nodeadkeys";
      options = "caps:ctrl_modifier";
    };

    # Touchpad (managed by libinput in hardware.nix)
    libinput.enable = true;

    # Qtile window manager
    windowManager.qtile = {
      enable = true;
      # Qtile with Python-based config
    };

    # XFCE desktop environment
    desktopManager.xfce = {
      enable = true;
      noDesktop = false;
    };

    # Display manager - using startx (configured in xinit.nix)
    displayManager = {
      # startx.enable is set in xinit.nix
      # LightDM as fallback option
      # lightdm.enable = true;
    };

    # Video drivers
    videoDrivers = ["modesetting"];
  };

  # Desktop portals for Wayland/X11 interoperability
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
    config = {
      common = {
        default = ["gtk"];
      };
    };
  };

  # D-Bus
  services.dbus.enable = true;



  # GTK settings
  programs.dconf.enable = true;

  # Fonts
  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      # Monospace
      comic-mono
      fira-code
      fira-code-symbols
      jetbrains-mono
      source-code-pro

      # Sans-serif
      fira
      fira-sans
      inter
      liberation_ttf

      # Serif
      liberation_ttf
      source-serif

      # Special fonts
      shantell-sans
      nerdfonts
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
    ];

    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = ["Fira Code" "Comic Mono"];
        sansSerif = ["Inter" "Fira Sans"];
        serif = ["Liberation Serif"];
        emoji = ["Noto Color Emoji"];
      };
      hinting = {
        enable = true;
        style = "full";
      };
      subpixel = {
        lcdfilter = "default";
      };
    };
  };

  # Qt theme configuration
  qt = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };

  # GTK theme and desktop packages
  environment.systemPackages = with pkgs; [
    # Themes
    gnome-themes-extra
    adwaita-gtk-theme
    gtk-engine-murrine
    gtk_engines

    # Icon themes
    faba-icon-theme
    faba-mono-icons
    paper-icon-theme
    papirus-icon-theme

    # Theme tools
    lxappearance
    qt5ct
    qt6ct

    # Desktop utilities
    xdg-utils
    shared-mime-info

    # Window manager utilities
    rofi
    rofi-calc
    rofi-emoji
    dunst
    libnotify
    polybar
    eww

    # Screen utilities
    arandr
    autorandr
    xrandr

    # Clipboard
    clipmenu
    xclip
    xsel

    # Screenshots
    flameshot
    scrot
    maim

    # Misc X tools
    xdotool
    xev
    xprop
    xwininfo
    xkill

    # i3lock for screen locking
    i3lock
  ];

  # Environment variables for theming
  environment.sessionVariables = {
    # GTK theme
    GTK_THEME = "Adwaita:dark";

    # Qt theme
    QT_QPA_PLATFORMTHEME = "gtk2";
    QT_STYLE_OVERRIDE = "gtk2";

    # Cursor
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "24";
  };

  # XDG directories
  xdg = {
    menus.enable = true;
    icons.enable = true;
    autostart.enable = true;
    sounds.enable = true;
    mime.enable = true;
  };

  # SDDM/LightDM not used (startx approach)
  # LightDM configuration if needed as fallback
  services.displayManager = {
    # No display manager - using startx
    autoLogin = {
      enable = false;
      user = "klingenberg";
    };
  };

  # Screensaver and screen locking
  services.xserver.xautolock = {
    enable = true;
    enableNotifier = true;
    locker = "${pkgs.i3lock}/bin/i3lock -c 000000";
    nowlocker = "${pkgs.i3lock}/bin/i3lock -c 000000";
    time = 10; # minutes
    notifier = "${pkgs.libnotify}/bin/notify-send 'Screen locking in 30 seconds'";
    notify = 30; # seconds before lock
  };

  # Redshift for night light
  services.redshift = {
    enable = true;
    temperature = {
      day = 6500;
      night = 3500;
    };
    latitude = "52.52";
    longitude = "13.405";
  };

  # Picom compositor (for transparency and shadows)
  services.picom = {
    enable = true;
    fade = true;
    fadeDelta = 5;
    shadow = true;
    shadowExclude = [
      "class_g = 'Dunst'"
      "class_g = 'Rofi'"
    ];
    backend = "glx";
    vSync = true;
  };

}