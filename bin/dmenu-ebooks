#!/bin/bash

pushd $HOME/Dropbox/ebooks
dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
book=$(fd . -t f | dmenu -l 10 -p "Book: " $dmenu_config)

if [ ! -z "$book" ]; then
    xdg-open "$HOME/Dropbox/ebooks/$book"
fi



