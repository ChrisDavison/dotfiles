function litmkpair
	set fn $argv[1]
    set renamed $fn
    if test (count $argv) -gt 1
        set renamedfull $argv[2]
        set renamedbase (string split -r '.' $renamedfull)
        set fn_ext (string split -r '.' $fn)[2]
        set renamed "$renamedbase.$fn_ext"
    end
    set tidy_fn (echo $renamed | tr '[[:upper:]]' '[[:lower:]]' | sed 's/ /-/g')
    set basefn (string split -r '.' $tidy_fn)[1]
    mv $fn $tidy_fn
    echo "# $renamed" > "$basefn".txt
    echo >> "$basefn".txt
    echo "@unread" >> "$basefn".txt
    mv "$basefn".txt > ./writeups/
    mv $tidyfn > ./papers/
end
