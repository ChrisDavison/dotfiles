function new_in_git
    set -l n_days $argv[1]
    echo "New files since $n_days days ago"
    echo "--------------------------------"
    git log --oneline --name-only --diff-filter=A --pretty="" --since="$n_days days"
end
