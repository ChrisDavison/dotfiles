cv() {
    if [ -d ~/.vim-sessions ]; then
        selected=$(fd . ~/.vim-sessions | fzf -q "$1")
        [[ -n "$selected" ]] && $EDITOR -S "$selected"
    else
        echo "Couldn't find ~/.vim-sessions folder"
    fi
}

venv() {
    if [ -d ~/.envs ]; then
        selected=$(fd "\bactivate$" ~/.envs | fzf -q "$1")
        [[ -n "$selected" ]] && source "$selected"
    else
        echo "Couldn't find ~/.envs folder"
    fi
}

choose_tmux_session() {
    if tmux list-sessions 2>&1 > /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
}

mcd() {
    # Make, and switch to, a directory
    if [ ! -n "$1" ]; then
        echo "Must pass directory as argument"
        return
    fi
    mkdir -p "$1"
    cd "$1"
}

md() {
    # Preview a markdown file
    if [ ! -n "$1" ]; then
        echo "Must pass file as argument"
        return
    fi
    pandoc --from gfm --to html "$1" > /tmp/md.html
    open /tmp/md.html
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

repofetch() { _for_each_repo git fetch --all }

repofunc() { _for_each_repo git "$@"
}

repostat() {
    [ ! $(pwd) = "$HOME" ] && pushd "$HOME" >> /dev/null
    echo "Unchanged repos will not be displayed"
    echo "-------------------------------------"
    for repo in $HOME/devel/*; do
        if [ ! -d "$repo" ]; then
            continue
        fi
        cd "$repo"
        git status -s -b > ~/.stat
        if [[ $(cat ~/.stat | wc -l) -gt 1 ]] || [[ $(cat ~/.stat | grep -E -e "ahead|behind" | wc -l) -gt 0 ]]; then
            echo "$(basename $repo)"
            cat ~/.stat
            echo "....................................."
        fi
    done
    popd
}

repobstat() {
    [ ! $(pwd) = "$HOME" ] && pushd "$HOME" >> /dev/null
    echo "Unchanged repos will not be displayed"
    echo "-------------------------------------"
    for repo in $HOME/devel/*; do
        if [ ! -d "$repo" ]; then
            continue
        fi
        cd "$repo"
        git branchstatus > ~/.stat
        if [[ $(cat ~/.stat | grep -E -e "ahead|behind|modified|untracked" | wc -l) -gt 0 ]]; then
            echo "+++ $(basename $repo)"
            cat ~/.stat
            echo "....................................."
        fi
    done
    popd
    
}

newgit() {
    dir="$1"
    mkdir "$dir"
    cd "$dir"
    git init
    echo "# $dir" >> README.md
    git add README.md
    git commit -m "Initial commit"
    git status
}

capture(){
    args="$@"
    d=$(date +"%F %T")
    if [ -f "$CAPTUREFILE" ]; then
        echo "- $d $args" >> "$CAPTUREFILE"
    else
        echo "Capturefile not defined"
    fi
}