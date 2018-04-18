export TODOFILE="$HOME/Dropbox/n/notes/todo.txt"
export EXTERNAL_BRAIN="$HOME/Dropbox/n/notes/capture.txt"
noteadd(){
    if [ -e "$EXTERNAL_BRAIN" ]; then
        echo "$(date +'%Y%m%d') $1" >> "$EXTERNAL_BRAIN"
    else
        echo "$(date +'%Y%m%d') $1" > "$EXTERNAL_BRAIN"
    fi
}

noteview(){
     if [ -e "$EXTERNAL_BRAIN" ]; then
        cat "$EXTERNAL_BRAIN"
    else
        echo "No notes.  Need to create $EXTERNAL_BRAIN"
    fi
}

noteclear(){
    [ -e "$EXTERNAL_BRAIN" ] && rm "$EXTERNAL_BRAIN" && touch "$EXTERNAL_BRAIN"
}

notefile(){
    echo "$EXTERNAL_BRAIN"
}

alias n="noteadd"
alias na="noteadd"
alias nv="noteview"
alias nc="noteclear"
alias t="todo.sh"
