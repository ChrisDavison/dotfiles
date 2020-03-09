#!/bin/bash
if [ -z "$@" ]; then
    function gen_bookmarks()
    {
        cat ~/.bookmarks | sort | cut -d';' -f1
    }
    gen_bookmarks
else
    site=$@
    if [ ! -z "$site" ]; then
        url=$(grep "$site" ~/.bookmarks | cut -d';' -f2)
        if [ ! -z "$url" ]; then
            xdg-open "$url" > /dev/null
        fi
    fi

fi
