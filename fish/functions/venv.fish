function venv --description "Choose a python virtualenv"
    if [ -d ~/.envs ]
        set -l selected (find ~/.envs -type f -regex ".*\/activate.fish" | fzf -q "$argv[1]")
        if [ -n "$selected" ]
            source "$selected"
        end
    else
        echo "Couldn't find virtualenvs"
    end
end
