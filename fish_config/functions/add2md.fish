function add2md
	set -l dest $argv[1]
    set -l destbase (dirname $dest)
    set -l file_dir "assets"
    set -l target $destbase/$file_dir
    if not test -f $dest
        echo "No note file: $dest"
        return 1
    end
    if not test -d $target
        echo "Made directory $target"
        mkdir $target
    end
    echo "Linking notes to $dest"
    echo >> $dest
    for fn in $argv[2..(count $argv)]
        set -l fn_short (basename $fn)
        mv $fn $target/$fn_short
        echo "- [$fn_short](./$file_dir/$fn_short)" >> $dest
    end
    echo
    echo "=== TAIL OF THE NOTE FILE ==="
    tail -n (math (count $argv) + 1) $dest
end
