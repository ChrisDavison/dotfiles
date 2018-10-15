function venv --description "Choose a python virtualenv"
    set -g selected ""
    if [ -d ~/.envs ]
        set -g selected (find ~/.envs -name "activate.fish" | fzf -1 -q "$argv[2]")
        if [ -n "$selected" ]
            source "$selected"
        else
            echo "Selected was empty"
        end
    else
        echo "Couldn't find virtualenvs"
    end
end
