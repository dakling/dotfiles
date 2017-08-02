#! /bin/zsh

maxVolume=100
currentVolume=$(pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')

if [[ $(($currentVolume + $1)) -le $maxVolume ]]; then
    pactl set-sink-volume $(pacmd list-sinks |awk '/* index:/{print $3}') $1%
fi
