nm-applet&
pamac-tray&
onboard&
touchegg&
pnmixer&
easystroke enable&
dropbox&
/usr/share/HESSENBOX_DA/HESSENBOX_DA-Client.sh&

# Disable access control
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# for guix
export PATH="/home/klingenberg/.guix-profile/bin${PATH:+:}$PATH"
export XDG_DATA_DIRS="/usr/share${PATH:+:}$PATH"
export INFOPATH="/home/klingenberg/.guix-profile/share/info${INFOPATH:+:}$INFOPATH"

# Themes, etc
gnome-settings-daemon &

# Fallback cursor
xsetroot -cursor_name left_ptr&

# Keyboard repeat rate
xset r rate 600 20&


exec emacs
# exec xmonad
