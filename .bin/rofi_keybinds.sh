#!/bin/bash
if [ -z "$@" ]; then
    function gen_keybinds()
    {
        ls -1 ~/src/github.com/ChrisDavison/dotfiles/keybind-helpers | rg ".md" | sort | cut -d'.' -f1
    }
    gen_keybinds
else
    site=$@
    if [ ! -z "$site" ]; then
        show-keybinds.sh $site
        # url=$(grep "$site" ~/.bookmarks | cut -d';' -f2)
        # if [ ! -z "$url" ]; then
        #     xdg-open "$url" > /dev/null
        # fi
    fi

fi
