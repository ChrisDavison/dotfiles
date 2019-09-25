#!/bin/bash

site=$(cat ~/.bookmarks | cut -d';' -f1 | dmenu -i -l 10 -p "Site: " -fn "Ubuntu Mono-14")

url=$(grep "$site" ~/.bookmarks | cut -d';' -f2)

if [ ! -z "$url" ]; then
    xdg-open "$url"
fi

