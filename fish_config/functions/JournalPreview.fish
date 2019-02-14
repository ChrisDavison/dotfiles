function JournalPreview
	cat $NOTESDIR/journal.md | 
        rg "^\*" | 
        fzf --preview 'echo {}' --preview-window right:50%:wrap -s --tac
end
