#!/usr/bin/env bash
# exit on error
set -e

notes_source="$HOME/Dropbox/notes/"
notes_dest="$HOME/src/github.com/ChrisDavison/knowledge/"

# sync everything from source (dropbox notes) to dest (github 'knowledge' repo)
rsync -r $notes_source $notes_dest

# change to the git repo and commit everything, with timestamp as commit message
git add --all
git commit -m "backup $(date +'%Y%m%d %H')"
git push
