function notes
    set -l query $argv[1]
    set -l batcmd "bat $NOTESDIR/{} --color=always -n"
    fd . -e md "$NOTESDIR" | sed -e "s!$NOTESDIR/!!g" | fzf -q "$query" --multi --preview="$batcmd" --preview-window=down:50% | sed -e "s!^!$NOTESDIR/!g"
end
