function mdhashtags
    set loc "$NOTESDIR"
    if [ $PWD/ = "$NOTESDIR"/* ]
        set loc "."
    end
    rg "(?:[\s\`^])#[a-zA-Z]+" "$loc" -g "*.md" -o --no-heading --sort=path
end
