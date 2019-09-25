#!/bin/bash

menu="lock
logout
suspend
hibernate
reboot
shutdown
"

args="--quality 100"
opt=$(dmenu -l 10 -fn "Ubuntu Mono-18" -p "Power: " <<< $menu | cut -d' ' -f1)

case "$opt" in
    lock) i3scrlock ;;
    logout) gnome-session-quit --logout ;;
    reboot) systemctl reboot ;;
    shutdown) systemctl poweroff ;;
    suspend) i3locksusp ;;
    *) exit 1 ;;
esac

