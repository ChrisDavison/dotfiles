#!/bin/sh

dir=~/.dotfiles/files

mkdir $dir

for file in $(ls -a -1 | grep -e "\..*" | grep -v ".*~")
do
    if [[ -f $file ]]; then
        echo $file;
        cp $file $dir
    fi
done
