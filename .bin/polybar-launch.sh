#!/usr/bin/env sh
pkill polybar
for m in `polybar --list-monitors | cut -d':' -f1`; do
    MONITOR=$m polybar -r example &
done
