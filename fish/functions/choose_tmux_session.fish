function choose_tmux_session --description "Choose a tmux session"
    set -l selected (tmux list-sessions | fzf -q "$1" | cut -d: -f1)
    if [ -n "$selected" ]
        tmux attach -d -t "$selected"
    end
end
