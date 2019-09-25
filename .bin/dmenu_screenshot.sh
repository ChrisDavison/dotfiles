#!/bin/bash

menu="select rectangle
focused window 
all displays
"

args="--quality 100"
opt=$(dmenu -l 10 -fn "Ubuntu Mono-18" -p "Screenshot mode: " <<< $menu | cut -d' ' -f1)

output_fn="$HOME/Dropbox/Camera Uploads/screenshots/%Y%m%d-%H%M%S.png" 
case "$opt" in
    focused) scrot -u $args $output_fn ;;
    all) scrot -m $args $output_fn ;;
    select) scrot -s $args $output_fn ;;
    *) ;;
esac
