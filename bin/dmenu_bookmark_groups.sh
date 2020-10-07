#!/bin/bash
# browser=google-chrome
browser=firefox
bmFile=$HOME/code/dotfiles/.bookmarks

dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi

awkMenuCmd="
/^ignore/{exit}
/^[a-zA-Z1-9]/{ # lines starting at left edge are headers
print  # print the remainder
}
"

option=$(awk "$awkMenuCmd" $bmFile | dmenu $dmenu_config -p "Bookmarks:")
if [ ! -z "$option" ]; then
    echo $option
    awkBmCmd="
    /^$option/{start=1; next} # find the matching header
    /^[a-zA-Z1-9]/ && start==1{exit}     # if we're printing, stop at next header
    /^#/{next}                  # skip commented bookmarks
    start == 1{gsub(/    .*;/, \"\"); print} # strip bookmark name
    "
    awk "$awkBmCmd" $bmFile | while read -r bookmark; do
        echo $bookmark
        [ ! -z "$bookmark" ] && $browser --new-tab "$bookmark"
    done
fi

