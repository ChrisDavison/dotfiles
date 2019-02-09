function mdimages
    set loc "$NOTESDIR"
    if [ $PWD/ = "$NOTESDIR"/* ]
        set loc "."
    end
    rg "!\[.*?\]\(.*?\)" "$loc" -g "*.md" -o --no-heading --sort=path
end
