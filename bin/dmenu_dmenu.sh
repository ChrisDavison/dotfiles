#!/bin/bash

declare -a options=("Bookmarks
Bluetooth
Config
Desktop (Applications)
DmenuDmenu
Powermenu
Screenshots
Window Switcher
")

dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
if [ $# -gt 0 ]; then
    choice=$1
else
    choice=$(echo -e "${options[@]}" | dmenu -p "Config:" -l 10 $dmenu_config)

 fi
   case $choice in
        Bookmarks) dmenu_bookmarks ;;
        Bluetooth) dmenu_bluetooth ;;
        "Desktop (Applications)") j4-dmenu-desktop --dmenu="dmenu -i -l 10 -m 0 -fn 'Ubuntu Mono-18' -p 'Program:'"
    ;;
        DmenuDmenu) ~/.bin/dmenu_dmenu ;;
        Powermenu) dmenu_powermenu.sh ;;
        Screenshots) dmenu_screenshot.sh ;;
        "Window Switcher") dmenu_win_switcher_i3 ;;
        *|quit) exit ;;
    esac

