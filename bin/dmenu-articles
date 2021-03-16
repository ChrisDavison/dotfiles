#!/bin/bash

pushd $HOME/Dropbox/literature
dmenu_config="-i -fn 'Hack-14' -sb #8620e6 -nhb #8620e6 -nhf #f438ee -shb #8620e6 -shf #ffffff"
book=$(fd . -t f | dmenu -l 10 -p "Article: " $dmenu_config)

if [ ! -z "$book" ]; then
    xdg-open "$HOME/Dropbox/literature/$book"
fi

