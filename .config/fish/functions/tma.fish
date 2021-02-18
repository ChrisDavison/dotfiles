function tma
    if string length -q "$TMUX"
        echo "ALREADY IN TMUX"
        return
    end
    set chosen (tmux ls | cut -d':' -f1 | fzf -0 -1)
    if string length -q "$chosen"
        tmux attach -t "$chosen"
    else
        tmux
    end
end
