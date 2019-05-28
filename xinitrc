#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# if [[ -f ~/.extend.xinitrc ]];then
# 	. ~/.extend.xinitrc
# else
# 	DEFAULT_SESSION=xfce4-session
# fi

# userresources=$HOME/.Xresources
# usermodmap=$HOME/.Xmodmap
# sysresources=/etc/X11/xinit/.Xresources
# sysmodmap=/etc/X11/xinit/.Xmodmap

# # merge in defaults and keymaps

# if [ -f $sysresources ]; then
#     xrdb -merge $sysresources
# fi

# if [ -f $sysmodmap ]; then
#     xmodmap $sysmodmap
# fi

# if [ -f "$userresources" ]; then
#     xrdb -merge "$userresources"
# fi

# if [ -f "$usermodmap" ]; then
#     xmodmap "$usermodmap"
# fi

# # start some nice programs

# if [ -d /etc/X11/xinit/xinitrc.d ] ; then
#     for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
#         [ -x "$f" ] && . "$f"
#     done
#     unset f
# fi

# get_session(){
# 	local dbus_args=(--sh-syntax --exit-with-session)
# 	case $1 in
# 		awesome) dbus_args+=(awesome) ;;
# 		bspwm) dbus_args+=(bspwm-session) ;;
# 		budgie) dbus_args+=(budgie-desktop) ;;
# 		cinnamon) dbus_args+=(cinnamon-session) ;;
# 		deepin) dbus_args+=(startdde) ;;
# 		enlightenment) dbus_args+=(enlightenment_start) ;;
# 		fluxbox) dbus_args+=(startfluxbox) ;;
# 		gnome) dbus_args+=(gnome-session) ;;
# 		i3|i3wm) dbus_args+=(i3 --shmlog-size 0) ;;
# 		jwm) dbus_args+=(jwm) ;;
# 		kde) dbus_args+=(startkde) ;;
# 		lxde) dbus_args+=(startlxde) ;;
# 		lxqt) dbus_args+=(lxqt-session) ;;
# 		mate) dbus_args+=(mate-session) ;;
# 		xfce) dbus_args+=(xfce4-session) ;;
# 		openbox) dbus_args+=(openbox-session) ;;
# 		*) dbus_args+=($DEFAULT_SESSION) ;;
# 	esac

# 	echo "dbus-launch ${dbus_args[*]}"
# }

# # exec $(get_session)
# # exec i3
# # trayer --edge top --align right --SetDockType true --SetPartialStrut true \
# #  --expand true --width 10 --transparent true --tint 0x191970 --height 12 &
# exec xmonad
# # exec emacs
# # exec awesome
# # exec xfce4-session
# source ~/.config/stumpwm/autostart.sh
# exec stumpwm

# # twm &
# # xclock -geometry 50x50-1+1 &
# # xterm -geometry 80x50+494+51 &
# # xterm -geometry 80x20+494-0 &
# #exec xterm -geometry 80x66+0+0 -name login
# # xset r rate 660 25


####
#guix
export PATH="/root/.guix-profile/bin${PATH:+:}$PATH"
export PATH="/home/klingenberg/.guix-profile/bin:/home/klingenberg/.guix-profile/sbin${PATH:+:}$PATH"
export INFOPATH="/home/klingenberg/.guix-profile/share/info${INFOPATH:+:}$INFOPATH"
export GUILE_LOAD_PATH="/home/klingenberg/.guix-profile/share/guile/site/2.2${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
export XDG_DATA_DIRS="/home/klingenberg/.guix-profile/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
export GIO_EXTRA_MODULES="/home/klingenberg/.guix-profile/lib/gio/modules${GIO_EXTRA_MODULES:+:}$GIO_EXTRA_MODULES"

# for exwm
#autostart
nm-applet&
pamac-tray&
dropbox&
pnmapplet&
xmodmap -e 'clear mod4'&
xmodmap -e 'add mod4 = Super_L'&
easystroke enable&
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1&
setxkbmap -option ctrl:nocaps  &
xcape -e 'Control_L=Escape'&
/usr/share/HESSENBOX_DA/HESSENBOX_DA-Client.sh start&
/usr/lib/notification-daemon-1.0/notification-daemon&
onboard&
touchegg&

# # Disable access control
xhost +SI:localuser:$USER

# # Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# # Themes, etc
# gnome-settings-daemon &

# # Fallback cursor
xsetroot -cursor_name left_ptr

# # Keyboard repeat rate
xset r rate 400 20

# # Start Emacs
exec emacs
# exec xmonad
