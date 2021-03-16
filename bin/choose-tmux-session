#!/usr/bin/env bash
if tmux list-sessions 2>&1 > /dev/null ; then
    selected=$(tmux list-sessions | cut -d: -f1 | fzf -q "$1")
    [[ -n "$selected" ]] && tmux attach -d -t "$selected"
else
    echo "No tmux sessions running."
fi
