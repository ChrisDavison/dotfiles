function in_each_repo
    for repo in (fd -t d -d 1 . $HOME/code)
        pushd $repo
        if test -d ".git"
            set -l out (git $argv)
            if test -n "$out"
                printf "%-20s %s\n" (basename $repo) $out
            end
        end
        popd
    end | sed -e "/^\$/d"
end
