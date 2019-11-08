#!/bin/bash

if [ -z "$@" ]; then
    function gen_directories()
    {
        find $HOME/src/github.com/chrisdavison -mindepth 1 -maxdepth 1 -type d -exec realpath --relative-to=$HOME/src/github.com/ {} \;
        find $HOME/src/github.com/cidcom -mindepth 1 -maxdepth 1 -type d -exec realpath --relative-to=$HOME/src/github.com/ {} \;
    }
    echo $(realpath --relative-to=$HOME "$HOME/Dropbox/notes/")
    gen_directories
else
    if [ -d "$HOME/$@" ]; then
        gnome-terminal --working-directory="$HOME/$@" -x nvim "$HOME/$@"
    elif [ -d "$HOME/src/github.com/$@" ]; then
        gnome-terminal  --working-directory="$HOME/src/github.com/$@" -x nvim "$HOME/src/github.com/$@"

    else
        echo "No $project"
    fi
fi
