function asmr
    set matching (asmrfind (string split " " $argv) | sort -R | head -n1)
    if [ -z "$matching" ]
        echo "No matching"
        return 1
    end
    set name (echo $matching | cut -d';' -f1)
    set vidid (echo $matching | cut -d';' -f2)
    echo "Watching: $name"
    echo "https://youtube.com/watch?v=$vidid" | OpenInBrowser
end
