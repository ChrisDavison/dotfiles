#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

MONITOR=LVDS1 polybar example &

hdmi1_available=$(xrandr -q | rg "HDMI1 disconnected" | wc -l)
if [ $hdmi1_available -eq 1 ]; then
    echo "HDMI1 not available"
    xrandr --output HDMI1 --off
else
    xrandr --output HDMI1 --above LVDS1 --auto --noprimary
    MONITOR=HDMI1 polybar example &
fi
