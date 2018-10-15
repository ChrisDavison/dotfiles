function cv --description "Choose a vim session"
    if [ -d ~/.vim-sessions ]
        set -l selected (find ~/.vim-sessions -name "*.vim" -type f | fzf -q "$argv[1]")
        if [ -n "$selected" ]
            eval $EDITOR -S "$selected"
        end
    else
        echo "Couldn't find ~/.vim-sessions folder"
    end
end

