#!/bin/bash

menu="latex
python
rust
pandoc
go
sklearn
c++
"

browser="firefox"
# browser="xdg-open"

dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
opt=$(dmenu -p "Reference: " $dmenu_config <<< $menu | cut -d' ' -f1)

case "$opt" in
    latex) $browser "https://en.wikibooks.org/wiki/LaTeX" ;;
    python) $browser "https://devdocs.io/python~3.7/" ;;
    rust) $browser "https://doc.rust-lang.org/std/" ;;
    pandoc) $browser "https://pandoc.org/MANUAL.html" ;;
    go) $browser "https://golang.org/pkg/" ;;
    sklearn) $browser "https://scikit-learn.org/stable/modules/classes.html" ;;
    c++) $browser "https://devdocs.io/cpp/" ;;
    *) ;;
esac

