function repos_fetch
	for repo in ~/devel/*
        pushd $repo
        set_color --bold magenta; echo -n "Fetching:" 
        set_color yellow; echo "$repo"
        set_color normal; git fetch --all
        echo
        popd
    end
end
