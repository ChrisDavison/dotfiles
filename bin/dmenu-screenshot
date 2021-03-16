#!/bin/bash

menu="focused window
all displays
select rectangle"

args="--quality 100"
dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
opt=$(dmenu $dmenu_config -p "Screenshot:" <<< $menu | cut -d' ' -f1)

output_fn="$HOME/Dropbox/Camera Uploads/screenshot--%Y%m%d-%H%M%S.png"
case "$opt" in
    focused) scrot -u $args "$output_fn" ;;
    all) scrot -m $args "$output_fn" ;;
    select) scrot -s $args "$output_fn" ;;
    *) ;;
esac
