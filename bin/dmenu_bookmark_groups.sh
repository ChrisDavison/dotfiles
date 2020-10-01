#!/bin/bash
# browser=google-chrome
browser=firefox
bmFile=$HOME/code/dotfiles/.bookmark-groups

dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi

awkMenuCmd="
/^-- /{ # lines starting '-- ' are section headers
gsub(/-- /, \"\"); # strip the leading '-- '
print  # print the remainder
}
"

option=$(awk "$awkMenuCmd" $bmFile | sort | dmenu $dmenu_config -p "Bookmarks:")
if [ ! -z "$option" ]; then
    echo $option
    awkBmCmd="
    /^-- $option/{start=1; next} # find the matching header
    /^-- / && start==1{exit}     # if we're printing, stop at next header
    /^#/{next}                  # skip commented bookmarks
    start == 1{gsub(/.*;/, \"\"); print} # strip bookmark name
    "
    awk "$awkBmCmd" $bmFile | while read -r bookmark; do
        [ ! -z "$bookmark" ] && $browser --new-tab "$bookmark"
    done
fi

