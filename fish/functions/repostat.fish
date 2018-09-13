function repostat
	for repo in ~/devel/*
        pushd $repo
        if [ (git status -s | wc -l) -gt 0 ]
            set_color --bold magenta; echo -n "Status of" 
            set -l reposhort (basename $repo)
            set -l branch (git rev-parse --abbrev-ref HEAD)
            set_color yellow; echo -n " $reposhort"
            set_color magenta; echo -n " on"
            set_color yellow; echo " $branch"
            set_color normal; git status -s
            echo
        end
        popd
    end
end
