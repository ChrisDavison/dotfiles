function fish_greeting
	cat $NOTESDIR/journal.md | rg "^\*" | tail -n1 | tr -d '*'
end
