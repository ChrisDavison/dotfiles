function new_in_git
    if test (count $argv) -eq 0
        set n_days 1
    else
        set n_days $argv[1]
    end
    echo "New files since $n_days days ago"
    echo "(excluding deleted files)"
    echo "--------------------------------"
    set -l files (git log --oneline --name-status --pretty="" --since="$n_days days" | sort -n | awk '/^[AM]/{print $2}' | uniq)
    for f in $files
        if test -f $f
            echo $f
        end
    end
end
