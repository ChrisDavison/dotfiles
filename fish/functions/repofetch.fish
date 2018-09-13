function repofetch
	for repo in ~/devel/*
        pushd $repo
        set_color --bold yellow; echo "$repo"
        set_color normal; git fetch --all
        echo
        popd
    end
end
