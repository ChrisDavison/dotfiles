#!/bin/bash

declare -a options=("alacritty
bspwm
fish
git
polybar
sxhkd")

choice=$(echo -e "${options[@]}" | dmenu -p "Config:" -i -l 10 -fn "Ubuntu Mono-18")

case $choice in
    alacritty) alacritty -e nvim ~/.config/alacritty/alacritty.yml ;;
    bspwm) alacritty -e nvim ~/.config/bspwm/bspwmrc ;;
    fish) alacritty -e nvim ~/.config/fish/config.fish ;;
    sxhkd) alacritty -e nvim ~/.config/sxhkd/sxhkdrc ;;
    polybar) alacritty -e nvim ~/.config/polybar/config ;;
    git) alacritty -e nvim ~/.gitconfig ;;
    *|quit) ;;
esac
