#!/usr/bin/env bash
dest=$1
dest_base=$(dirname $dest)
file_dir="assets"
target=$dest_base/$file_dir
[ ! -f "$dest" ] && echo "No note file: $dest" && return 1
[ ! -d "$target" ] && echo "No dir: $target" && return 2
shift
echo "Linking notes to $dest"
for fn in $@
do
    fn_short=$(basename $fn)
    echo "Move $fn_short to $target"
    echo "- [$fn_short](./$file_dir/$fn_short)" >> $dest
done
echo "===== TAIL OF THE NOTE FILE ====="
tail -n $(( $# + 2 )) $dest

