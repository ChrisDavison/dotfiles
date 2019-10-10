#!/bin/bash

if [ -z "$@" ]; then
    function gen_directories()
    {
        find $CODEDIR -mindepth 1 -maxdepth 1 -type d -exec realpath --relative-to=$HOME {} \;
    }
    echo $(realpath --relative-to=$HOME "$HOME/Dropbox/notes/")
    echo $(realpath --relative-to=$HOME "$HOME/Dropbox/thesis/analysis")
    gen_directories
else
    project=$HOME/$@
    if [ -d "$project" ]; then
        code "$project"
    else
        echo "No $project"
    fi
fi
