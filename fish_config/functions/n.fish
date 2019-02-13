function n
    if [ (count $argv) -eq 0 ]
        set file (find $NOTESDIR -name "*.md" |
            sed -e "s|$NOTESDIR||" | \
            sed -e "s|\.md\$||" | \
            fzf --multi --select-1 --exit-0 --preview="cat $NOTESDIR/{}.md" \
                --preview-window=right:70%:wrap)
        set fn $NOTESDIR$file".md"
        if [ -n $file ]
            $EDITOR $fn
        end
    else
        if [ $argv[1] = "-d" ]
            if [ -f $NOTESDIR"/"$argv[2]".md" ]
                rm $NOTESDIR/$argv[2]".md"
            end
        else
            $EDITOR $NOTESDIR$argv".md"
        end
    end
end

