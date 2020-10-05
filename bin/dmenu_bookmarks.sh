#!/bin/bash
browser=firefox
bmFile=$HOME/code/dotfiles/.bookmarks

dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi

awkMenuCmd="
/;/{
    gsub(/;.*/, \"\"); # strip the leading '-- '
    gsub(/^    /, \"\");
    print  # print the remainder
}
"

option=$(awk "$awkMenuCmd" $bmFile | sort | uniq | dmenu $dmenu_config -p "Bookmarks:")
if [ ! -z "$option" ]; then
    echo $option
    awkBmCmd="
    /    $option/{
        gsub(/.*;/, \"\"); # strip the leading '-- '
        print;  # print the remainder
        exit
    } # find the matching header
    "
    awk "$awkBmCmd" $bmFile | while read -r bookmark; do
        [ ! -z "$bookmark" ] && $browser --new-tab "$bookmark"
    done
fi

