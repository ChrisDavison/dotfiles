#!/usr/bin/env bash

filename=$1
in=$HOME/src/github.com/ChrisDavison/dotfiles/keybind-helpers/$1.md
out=$HOME/src/github.com/ChrisDavison/dotfiles/keybind-helpers/$1.html

if [ ! -f $in ]; then
    notify-send "No keybinds for $1"
else
    pandoc $in -o $out
    xdg-open $out > /dev/null
fi
