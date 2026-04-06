# Xinit/startx configuration
# Manages X session startup without display manager

{pkgs, ...}: {
  # Xserver auto-login bypass (we use startx)
  services.xserver.displayManager.startx.enable = true;

  # Xprofile configuration for startx
  environment.etc."xprofile".text = ''
    # Xprofile - sourced by startx
    # Set environment variables
    export XDG_SESSION_TYPE=x11
    export XDG_CURRENT_DESKTOP=qtile
    export GTK_THEME=Adwaita:dark
    export QT_QPA_PLATFORMTHEME=gtk2

    # Set keyboard layout and options
    setxkbmap de nodeadkeys -option caps:ctrl_modifier

    # Touchpad configuration (alternative to libinput)
    # xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1
    # xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 1
    # xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Disable While Typing Enabled" 1

    # Xrandr dual monitor setup
    # Uncomment and modify for your monitors
    # xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal \
    #        --output HDMI-1 --mode 1920x1080 --pos 1920x0 --rotate normal

    # Start services
    nm-applet --indicator &
    blueman-applet &
    pasystray &
    dunst &
    picom &
    flameshot &

    # Start redshift
    redshift-gtk &

    # Start polkit agent
    ${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1 &

    # Start screensaver
    xautolock -detectsleep &
  '';

  # Xinitrc template
  environment.etc."xinitrc".text = ''
    #!/bin/bash
    # Xinitrc - startx startup script

    # Source xprofile
    [ -f /etc/xprofile ] && source /etc/xprofile
    [ -f ~/.xprofile ] && source ~/.xprofile

    # Xresources
    [ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

    # Set keyboard repeat rate
    xset r rate 300 50

    # Disable screensaver (we use xautolock instead)
    xset s off
    xset -dpms

    # Set cursor
    xsetroot -cursor_name left_ptr

    # Start window manager
    # Default to Qtile, can be overridden via startx argument
    session=''${1:-qtile}

    case $session in
      qtile)
        exec qtile start
        ;;
      xfce)
        exec startxfce4
        ;;
      *)
        exec $session
        ;;
    esac
  '';

  # Xresources defaults
  environment.etc."X11/Xresources".text = ''
    ! Xresources defaults
    Xft.dpi: 96
    Xft.antialias: 1
    Xft.hinting: 1
    Xft.hintstyle: hintfull
    Xft.rgba: rgb
    Xft.lcdfilter: lcddefault

    ! Cursor
    Xcursor.theme: Adwaita
    Xcursor.size: 24
  '';

  # Xmodmap for keyboard customization
  environment.etc."X11/Xmodmap".text = ''
    ! Xmodmap - keyboard customization
    ! Caps Lock as Control (also set via setxkbmap)
    ! clear Lock
    ! keycode 66 = Control_L
    ! add Control = Control_L

    ! Additional key mappings can go here
  '';

  # System packages for X session
  environment.systemPackages = with pkgs; [
    xorg.xinit
    xorg.xset
    xorg.xsetroot
    xorg.xmodmap
    xorg.xrdb
    xorg.xinput
    xorg.xev
    xorg.xrandr
    xorg.xprop
    xorg.xwininfo
  ];
}