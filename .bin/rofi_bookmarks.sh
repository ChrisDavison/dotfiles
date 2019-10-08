#!/bin/bash

site=$(cat ~/.bookmarks | cut -d';' -f1 | rofi -dmenu)

if [ ! -z "$site" ]; then
    url=$(grep "$site" ~/.bookmarks | cut -d';' -f2)
    xdg-open "$url"
fi



