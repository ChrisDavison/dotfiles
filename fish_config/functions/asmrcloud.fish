function asmrcloud
    cat $ASMRFILE | 
        cut -d':' -f2 | 
        cut -d';' -f1 | 
        tr -d "'" |
        tr -cs '[:alpha:]' \n | 
        tr '[:upper:]' '[:lower:]' | 
        sort | 
        uniq |
        gshuf  |
        awk 'length($0) > 3' |
        tr "\n" ","
end
