#!/bin/bash
args="--quality 100"
opt=$(dmenu -l 10 -fn "Ubuntu Mono-18" -p "Screenshot mode: " <<< $(ls -1 ~/.cheatsheets) | cut -d' ' -f1)

feh $HOME/.cheatsheets/$opt
