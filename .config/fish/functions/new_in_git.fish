function new_in_git
    if test (count $argv) -eq 0
        set n_days 1
    else
        set n_days $argv[1]
    end
    echo "New files since $n_days days ago"
    echo "--------------------------------"
    git log --oneline --name-only --diff-filter=A --pretty="" --since="$n_days days" | sort -n
end
