#!/usr/bin/env bash
# exit on error
set -e 

backup_source_to_dest() {
    src=$1
    dest=$2
    echo "Source: $src"
    echo "Destination: $dest"
    echo "Backup date: $(date +'%Y%m%d %H')"
    fd -E ".git" -x git rm -rf {}
    rsync -r "$src" "$dest"
    git add --all
    git commit -m "backup $(date +'%Y%m%d %H')"
    git push
}

notes() {
    src="$HOME/Dropbox/notes/"
    dest="$HOME/src/github.com/ChrisDavison/knowledge/"
    cd "$dest"
    backup_source_to_dest "$src" "$dest"
}

logbook() {
    src="$HOME/Dropbox/logbook/"
    dest="$HOME/src/github.com/ChrisDavison/logbook/"
    cd "$dest"
    backup_source_to_dest "$src" "$dest"
}

curdir=`pwd`
notes
logbook
cd "$curdir"
