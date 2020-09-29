#!/bin/bash

dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
site=$(cat ~/.bookmarks | cut -d';' -f1 | dmenu -l 10 -p "Site: " $dmenu_config)

if [ ! -z "$site" ]; then
    url=$(grep "$site" ~/.bookmarks | cut -d';' -f2)
    xdg-open "$url"
fi



