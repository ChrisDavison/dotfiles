#!/usr/bin/env bash
set -euo pipefail

active=`pactl list short sinks | rg RUNNING | rg -v Pulse | cut -f2`

active_volume(){
    awk_cmd="/Name: $active/{start=1}
/Base Volume/{next}
start==1 && /Volume/{print}
start==1 && /^Sink/{exit}"
    activeVol=`pactl list sinks | awk "$awk_cmd" | sed -e 's/.* \(-*[0-9.]* dB\).*/\1/g' | sed -e 's/.[0-9]\+ dB.*//g' `
}

cmd="$1"

case "$cmd" in
    --up)
        active_volume
        if [ $activeVol -gt -7 ]; then
            pactl set-sink-volume $active 0dB
        else
            pactl set-sink-volume $active +3dB
        fi

        ;;
    --down) pactl set-sink-volume $active -3dB ;;
    --mute) pactl set-sink-mute $active toggle ;;
    *) echo "Unknown volume.sh command: $cmd" ;;
esac

