function asmrfind
    if [ -z "$ASMRFILE" ]
        echo "Need to define ASMRFILE"
        return 1
    end
    set args (string split " " $argv)
    set query ".*$args"
    if [ "$argv[1]" = "+" ]
        set query "^\+.*$argv[2..-1]"
    end
    set output (cat -s "$ASMRFILE" | sort | rg "$query")
    echo "$output"
end
