#!/bin/bash
args="--quality 100"

dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
opt=$(dmenu -l 10 -p "Screenshot mode: " $dmenu_config <<< $(ls -1 ~/.cheatsheets) | cut -d' ' -f1)

feh $HOME/.cheatsheets/$opt
