
export EDITOR=emacsclient
export ALTERNATE_EDITOR=vim
export VISUAL=emacsclient

export GTK_THEME="Adwaita:dark"
export WINIT_X11_SCALE_FACTOR=1
export DOOMDIR=~/.config/doom/
export EMACSDIR=~/.config/emacs/
# export BOSSS_INSTALL=~/BoSSS-experimental/
export QT_QPA_PLATFORMTHEME=qt5ct
export DOTNET_CLI_TELEMETRY_OPTOUT=1
# export MYCROFT_START_CMD="/usr/share/mycroft-core/start-mycroft.sh all"
export PDFVIEWER_texdoc="emacsclient"

export OMP_NUM_THREADS=1

export PATH=~/.local/bin/:$PATH
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
# export GDK_CORE_DEVICE_EVENTS=1

# # for dedalus3
# export MPI_PATH=/usr/bin/
# export FFTW_PATH=/usr/bin/

# # for ollama
# export OLLAMA_NUM_GPU=999
# export no_proxy=localhost,127.0.0.1
# export ZES_ENABLE_SYSMAN=1
# source /opt/intel/oneapi/setvars.sh
# # [optional] under most circumstances, the following environment variable may improve performance, but sometimes this may also cause performance degradation
# export SYCL_PI_LEVEL_ZERO_USE_IMMEDIATE_COMMANDLISTS=1
# # [optional] if you want to run on single GPU, use below command to limit GPU may improve performance
# export ONEAPI_DEVICE_SELECTOR=level_zero:0

xmodmap ~/.Xmodmap &

# read -t 3 -p "press x for xfce" -n1 desktop_environment
sleep 3

~/.screenlayout/default.sh || ~/.screenlayout/double.sh || ~/.screenlayout/single.sh

dropbox &

xset s off # disable screen saver blanking

setxkbmap de nodeadkeys &

synclient TouchpadOff=0
synclient TapButton1=1
synclient TapButton2=3
synclient TapButton3=2
synclient PalmDetect=1

# syndaemon -i 0.5 -t -K -R &

# exec startxfce4
# exec stumpwm

# if [ "${XDG_VTNR}" -eq 1 ]; then
#     exec stumpwm
# fi

if [ "${XDG_VTNR}" -eq 2 ]; then
    exec startxfce4
fi

# exec stumpwm
exec qtile start

sleep 60
    
# if [[ $desktop_environment == "x" ]]
# then
#     exec startxfce4
# else
#     exec stumpwm
# fi


# xmodmap -e 'clear mod4'&
# xmodmap -e 'add mod4 = Super_L'&
# pkill dropbox&
# dropbox&
# nm-applet&
# udiskie --tray&
# blueman-applet&
# pa-applet&
# /usr/bin/polkit-dumb-agent&
# setxkbmap de -option ctrl:nocaps nodeadkeys&
# xcape -e 'Control_L=Escape'&
# bash ~/.screenlayout/default.sh&
# "feh --bg-scale ~/Pictures/arch-bg.jpg"&

# emacs --daemon -f exwm-enable
# exec emacsclient -c

# exec emacs --eval "(my/exwm-setup)"
