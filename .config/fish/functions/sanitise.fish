function sanitise
    set num (count $argv)
    if [ $num -eq 0 ]
        echo "usage: sanitise <filename>"
        return
    end
    echo (basename "$argv[1..$num]") | tr '[:upper:]' '[:lower:]' | sed 's/[^a-zA-Z0-9.-]/-/g'  | tr -s - - | sed 's/\-$//g'
end
