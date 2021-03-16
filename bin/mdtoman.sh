#!/usr/bin/env bash

f_in=$1
f_out=${f_in%%.*}.1

pandoc --standalone --from markdown --to man $f_in --output $f_out
sudo mv $f_out /usr/local/share/man/man1/

