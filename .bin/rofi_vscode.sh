#!/bin/bash

if [ -z "$@" ]; then
    function gen_directories()
    {
        find $CODEDIR -type d -maxdepth 1 -exec basename {} \;
    }
    echo $(basename "~/Dropbox/notes/"); gen_directories
else
    project=$@
    if [ ! -z "$project" ]; then
        project_dir="$HOME/Dropbox/$project"
        if [ -d "$HOME/Dropbox/$project" ]; then
            code "$HOME/Dropbox/$project" 
        elif [ -d "$CODEDIR/$project" ]; then
            code "$CODEDIR/$project"
        else
            echo "No $project_dir"
        fi
    fi
fi
