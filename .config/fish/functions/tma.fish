function tma
    if string length -q "$TMUX"
        echo "ALREADY IN TMUX"
        return
    end
    set chosen (command tmux ls | cut -d':' -f1 | fzf -0 -1)
    if string length -q "$chosen"
        command tmux attach -t "$chosen"
    else
        command tmux
    end
end
