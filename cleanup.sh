for fn in bashrc gitconfig sqliterc tmux.conf vimrc vim; do
    target="$HOME/.$fn"
    if [ -f "$target" ]; then
        rm "$target" || echo "Error removing '$target'"
    elif [ -d "$target" ]; then
        rm -rf "$target" || echo "Error removing '$target'"
    fi
done
