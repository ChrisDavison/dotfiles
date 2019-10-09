#!/bin/bash

menu="lock
logout
suspend
hibernate
reboot
shutdown
"

if [ -z "$@" ]; then
    cat <<< $menu  
else
    option=$@
    if [ ! -z "$option" ]; then
        case "$option" in
            lock) i3scrlock ;;
            logout) gnome-session-quit --logout ;;
            reboot) systemctl reboot ;;
            shutdown) systemctl poweroff ;;
            suspend) i3locksusp ;;
            *) exit 1 ;;
        esac
    fi
fi
