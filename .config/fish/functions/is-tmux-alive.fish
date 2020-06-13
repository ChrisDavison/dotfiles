function is_tmux_alive
    if [ (tmux list-sessions | wc -l) -gt 0 ]
        echo "TMUX"
    else
        echo "noMUX"
    end
end
