function asmri
    if [ (count $argv) -gt 0 ]
        set matching (asmrfind | fzf -e -q $argv)
    else
        set matching (asmrfind | fzf -e)
    end
    if [ -z $matching ]
        echo "No match"
        return 1
    end
    set name (echo $matching | cut -d';' -f1)
    set vidid (echo $matching | cut -d';' -f2)
    echo "Watching: $name"
    echo "https://youtube.com/watch?v="$vidid | OpenInBrowser 
end
