#!/bin/bash

declare -a options=("alacritty
bspwm
fish
git
polybar
sxhkd")

dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
choice=$(echo -e "${options[@]}" | dmenu -p "Config:" -l 10 $dmenu_config)

case $choice in
    alacritty) alacritty -e nvim ~/.config/alacritty/alacritty.yml ;;
    bspwm) alacritty -e nvim ~/.config/bspwm/bspwmrc ;;
    fish) alacritty -e nvim ~/.config/fish/config.fish ;;
    sxhkd) alacritty -e nvim ~/.config/sxhkd/sxhkdrc ;;
    polybar) alacritty -e nvim ~/.config/polybar/config ;;
    git) alacritty -e nvim ~/.gitconfig ;;
    *|quit) ;;
esac
