#!/bin/bash

menu="lock
logout
suspend
hibernate
reboot
shutdown"

args="--quality 100"
dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
opt=$(dmenu -l 10 -p "Power: " $dmenu_config <<< $menu | cut -d' ' -f1)

case "$opt" in
    lock) i3scrlock ;;
    logout) gnome-session-quit --logout ;;
    reboot) systemctl reboot ;;
    shutdown) systemctl poweroff ;;
    suspend) i3locksusp ;;
    *) exit 1 ;;
esac

