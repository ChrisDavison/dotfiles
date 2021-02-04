#!/usr/bin/env bash
set -e

direc="$HOME/Dropbox/notes"
direc="$HOME/code/knowledge"

pushd $direc > /dev/null

with_booknotes=0
[[ "$1" == "-b" ]] && with_booknotes=1
[[ "$1" == "-s" ]] && with_booknotes=1

main() {
    previous_file="$1"
    file_to_edit=`select_file $previous_file`

    if [ -n "$file_to_edit" ] ; then
        "$EDITOR" "$file_to_edit"
        main "$file_to_edit"
    fi
}

select_file() {
    given_file="$1"
    if [[ $with_booknotes == 1 ]]; then
        fd . -e md | fzf --preview="bat {}" --preview-window=right:70%:wrap --query="$given_file"
    else
        fd . -e md | rg -v "book-notes|snippets" | fzf --preview="bat {} --color=always" --preview-window=right:70%:wrap --query="$given_file"
    fi
}

main ""

popd
