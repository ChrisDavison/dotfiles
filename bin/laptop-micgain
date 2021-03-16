#!/usr/bin/env bash
set -euo pipefail

cmd="$1"
source="@DEFAULT_SOURCE@"

case "$cmd" in
    --set) pactl set-source-volume "$source" $2 ;;
    --zero) pactl set-source-volume "$source" 0dB ;;
    --up) pactl set-source-volume "$source" +3dB ;;
    --down) pactl set-source-volume "$source" -3dB ;;
    *) echo "Unknown micgain.sh command: $cmd" ;;
esac

