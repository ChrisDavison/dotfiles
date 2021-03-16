#!/usr/bin/env bash
set -e

pushd $HOME/Dropbox/notes > /dev/null

[[ "$#" = 0 ]] && echo "usage: rgfn <query>" && exit

file_to_edit=$(rg "$@" -l -g '!tags' | fzf --preview="echo '===== Only context =====' && echo && rg $@ -C 5 {}" --preview-window=right:70%:wrap)
if [ -n "$file_to_edit" ] ; then
    "$EDITOR" "$file_to_edit"
fi

popd
