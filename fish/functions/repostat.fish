function repostat
	for repo in ~/devel/*
        pushd $repo
        set -g show 0
        git status -s -b > ~/.stat
        # set -g gstatus (git status -s -b)
        set -g lineschanged (cat ~/.stat | wc -l)
        set -g linesbehind (cat ~/.stat | grep -E -e "ahead|behind" | wc -l)
        if [ $lineschanged -gt 1 ]; or [ $linesbehind -gt 0 ]
            set_color --bold magenta; echo -n "Status of" 
            set -l reposhort (basename $repo)
            set -l branch (git rev-parse --abbrev-ref HEAD)
            set_color yellow; echo -n " $reposhort"
            set_color magenta; echo -n " on"
            set_color yellow; echo " $branch"
            set_color normal; cat ~/.stat
            echo 
        end
        popd
    end
end
