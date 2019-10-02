#!/bin/bash

site=$(cat ~/.bookmarks | cut -d';' -f1 | dmenu -i -l 10 -p "Site: " -fn "Ubuntu Mono-14")

if [ ! -z "$site" ]; then
    url=$(grep "$site" ~/.bookmarks | cut -d';' -f2)
    xdg-open "$url"
fi



