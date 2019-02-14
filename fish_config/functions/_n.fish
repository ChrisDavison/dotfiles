function _n
	set match (fd . -e md $NOTESDIR | 
        sed -e "s|$NOTESDIR/||" | 
        sed -e "s/\.md\$//" | 
        fzf --preview="cat --color=always $NOTESDIR/{}.md" --preview-window=bottom:50%:wrap -e)
    echo $NOTESDIR/$match".md"
end
