function repofunc
	for repo in ~/devel/*
        pushd $repo
        set_color --bold magenta; echo -n "Running"
        set_color blue; echo -n " $argv" 
        set_color magenta; echo -n " on"
        set_color yellow; echo " $repo"
        set_color normal; git $argv
        echo
        popd
    end
end
