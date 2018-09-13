function repostat
	for repo in ~/devel/*
        pushd $repo
        set -g show 0
        if [ (git status -s | wc -l) -gt 0 ]
            set show 1
        else if [ (git status -s -b | grep -E -e "ahead|behind" | wc -l) -gt 0 ]
            set show 1
        end
        if [ $show -eq 1 ]
            set_color --bold magenta; echo -n "Status of" 
            set -l reposhort (basename $repo)
            set -l branch (git rev-parse --abbrev-ref HEAD)
            set_color yellow; echo -n " $reposhort"
            set_color magenta; echo -n " on"
            set_color yellow; echo " $branch"
            set_color normal; git status -s -b
            echo 
        end
        popd
    end
end
