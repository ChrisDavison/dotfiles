function free_space_on
    set -l serv $argv[1]
    echo $serv
    ssh $serv "df -H | grep '/\$\|media'"
end
