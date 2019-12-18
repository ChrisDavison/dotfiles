#!/usr/bin/env bash
# exit on error
set -e 

backup_source_to_dest() {
    src=$1
    dest=$2
    echo "Source: $src"
    echo "Destination: $dest"
    echo "Backup date: $(date +'%Y%m%d %H')"
    pushd "$dest"
    fd -E ".git/" -x rm -rf {}
    rsync -r $source $dest
    git add --all
    git commit -m "backup $(date +'%Y%m%d %H')"
    git push
    popd
}

notes() {
    src="$HOME/Dropbox/notes/"
    dest="$HOME/src/github.com/ChrisDavison/knowledge/"
    backup_source_to_dest "$src" "$dest"
}

logbook() {
    src="$HOME/Dropbox/logbook/"
    dest="$HOME/src/github.com/ChrisDavison/logbook/"
    backup_source_to_dest "$src" "$dest"
}

notes
logbook
