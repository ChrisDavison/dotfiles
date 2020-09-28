#!/bin/bash

site=$(cat ~/.bookmarks | cut -d';' -f1 | dmenu -i -l 10 -p "Site: " -fn "Hack-14")

if [ ! -z "$site" ]; then
    url=$(grep "$site" ~/.bookmarks | cut -d';' -f2)
    xdg-open "$url"
fi



