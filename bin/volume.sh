#!/usr/bin/env bash
set -euo pipefail

active=`pactl list short sinks | rg RUNNING | cut -f1`
case "$1" in
    --up) pactl set-sink-volume $active +6dB ;;
    --down) pactl set-sink-volume $active -6dB ;;
    --mute) pactl set-sink-mute $active toggle ;;
esac
