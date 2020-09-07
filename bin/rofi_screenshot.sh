#!/bin/bash

menu="focused window
all displays
select rectangle"

if [ -z "$@" ]; then
    cat <<< $menu  
else
    option=$@
    args="--quality 100"
    output_fn="$HOME/Dropbox/Camera Uploads/screenshot--%Y%m%d-%H%M%S.png"
    if [ ! -z "$option" ]; then
        case "$option" in
            focused) scrot -u $args "$output_fn" ;;
            all) scrot -m $args "$output_fn" ;;
            select) scrot -s $args "$output_fn" ;;
            *) ;;
        esac
    fi
fi
