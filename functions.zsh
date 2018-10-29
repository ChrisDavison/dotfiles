cv() { # Choose a Vim session
    if [ -d ~/.vim-sessions ]; then
        selected=$(fd . ~/.vim-sessions | fzf -q "$1")
        [[ -n "$selected" ]] && $EDITOR -S "$selected"
    else
        echo "Couldn't find ~/.vim-sessions folder"
    fi
}

venv() { # Choose a python env in ~/.envs
    if [ -d ~/.envs ]; then
        selected=$(fd "\bactivate$" ~/.envs | fzf -q "$1")
        [[ -n "$selected" ]] && source "$selected"
    else
        echo "Couldn't find ~/.envs folder"
    fi
}

envv() { # Activate a virtual env from the root of this git repo
    find $(git rev-parse --show-toplevel) -regex ".*activate$"
}

choose_tmux_session() {
    if tmux list-sessions 2>&1 > /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
}

mcd() { # Make a directory, then switch to it
    # Make, and switch to, a directory
    if [ ! -n "$1" ]; then
        echo "Must pass directory as argument"
        return
    fi
    mkdir -p "$1"
    cd "$1"
}

_for_each_repo() {
    pushd "$HOME" >> /dev/null
    for repo in $HOME/devel/*; do
        if [ ! -d "$repo" ]; then
            continue
        fi
        cd "$repo"
        echo "Running $@ on $repo"
        $@
        echo
    done
    popd
}

copy_to_bin() { # Copy a file to ~/bin, without file extension
    cp "$1" ~/bin/$(noext $1)
}

link_to_bin() {
    ln -s "$1" ~/bin/$(noext $1)
}

newgit() { # Create a new project (touching readme and init commit)
    dir="$1"
    mkdir "$dir"
    cd "$dir"
    git init
    echo "# $dir" >> README.md
    git add README.md
    git commit -m "Initial commit"
    git status
}

capture(){ # Add a dated entry to file $CAPTUREFILE
    args="$@"
    d=$(date +"%F %T")
    if [ -f "$CAPTUREFILE" ]; then
        echo "- $d $args" >> "$CAPTUREFILE"
    else
        echo "Capturefile not defined"
    fi
}

list_dotfile_functions() {
    rg ".*\(\).*\{" ~/devel/dotfiles
}
