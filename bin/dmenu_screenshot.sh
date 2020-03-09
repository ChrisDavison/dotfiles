#!/bin/bash

menu="Latex
Python
Rust
Pandoc
Go
"

opt=$(dmenu -l 10 -fn "Ubuntu Mono-18" -p "Reference: " <<< $menu | cut -d' ' -f1)

case "$opt" in
    latex) firefox https://en.wikibooks.org/wiki/LaTeX
;;
    all) scrot -m $args "$output_fn" ;;
    select) scrot -s $args "$output_fn" ;;
    *) ;;
esac
