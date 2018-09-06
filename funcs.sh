running(){
    ps | tr -s " " | cut -d' ' -f 3- | awk 'NR>1{print}'
}

gg(){
    git grep -a -i "$1"
}

gga(){
    git grep -a -i "$1" $(git rev-list --all)
}

cv() {
    selected=$(find ~/.vim-sessions -name "*.vim" -type f | fzf -q "$1")
    [[ -n "$selected" ]] && $EDITOR -S "$selected"
}

choose_tmux_session() {
    if tmux list-sessions 2>&1 >> /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
}
alias tma=choose_tmux_session

