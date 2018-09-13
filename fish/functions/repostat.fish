function repostat
	for repo in ~/devel/*
        pushd $repo
        set_color --bold magenta; echo -n "Status of" 
        set_color yellow; echo " $repo"
        set_color normal; git status -s -b
        echo
        popd
    end
end
