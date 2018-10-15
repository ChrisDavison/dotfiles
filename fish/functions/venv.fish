function venv --description "Choose a python virtualenv"
    set -g selected ""
    if [ -d ~/.envs ]
        if [ -x (which fd) ]
            set -g selected (fd "activate.fish" ~/.envs | fzf -q "$argv[1]")
        else
            set -g selected (find ~/.envs -name "activate.fish" | fzf -q "$argv[2]")
        end
        if [ -n "$selected" ]
            source "$selected"
        else
            echo "Selected was empty"
        end
    else
        echo "Couldn't find virtualenvs"
    end
end
