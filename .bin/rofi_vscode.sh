#!/bin/bash

if [ -z "$@" ]; then
    function gen_directories()
    {
        find $HOME/src/github.com/chrisdavison -mindepth 1 -maxdepth 1 -type d -exec realpath --relative-to=$HOME {} \;
        find $HOME/src/github.com/cidcom -mindepth 1 -maxdepth 1 -type d -exec realpath --relative-to=$HOME {} \;
    }
    echo $(realpath --relative-to=$HOME "$HOME/Dropbox/notes/")
    gen_directories
else
    if [ -d "$HOME/$@" ]; then
        code "$HOME/$@"
    elif [ -d "$HOME/src/github.com/$@" ]; then
        code "$HOME/src/github.com/$@"
    else
        echo "No $project"
    fi
fi
