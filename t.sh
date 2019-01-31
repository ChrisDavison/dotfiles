_usage="usage: t <CMD> [ARGS...]

Modifying:
    a|add TEXT...    Add a task
    rm #NUM          Remove item #NUM
    do #NUM          Mark item #NUM as done
    undo #NUM        Move item #NUM from DONEFILE into TODOFILE

Viewing:
    ls [QUERY]       List tasks (optionally filtered)
    lsp [QUERY]      List prioritised tasks (optionally filtered)
    c|contexts       List all unique contexts '+CONTEXT'
    p|projects       List all unique projects '@PROJECT'

Filtered views:
    cl|contextless            Tasks without a context
    pl|projectless            Tasks without a project
    done                      List done tasks"

cmd=$1
shift

[ -z "$TODOFILE" ] && echo "TODOFILE not defined" && return 1
[ -z "$DONEFILE" ] && echo "DONEFILE not defined" && return 2

cp "$TODOFILE" "$TODOFILE.bak"

function main {
    case $cmd in
        a|add) echo "- $@" >> "$TODOFILE" ;;
        ls) todoslist "$@" ;;
        lsp) todoslist "\d\s+!.*$@" ;;
        c|contexts)
            print_todo_count
            rg -No "@(.+?)\b" "$TODOFILE" | sort | uniq -c
            echo
            echo "$(rg -v -No "@(.+?)\b" "$TODOFILE" | wc -l) with no context" ;;
        p|projects)
            print_todo_count
            rg -No "\+(.+?)\b" "$TODOFILE" | sort | uniq -c
            echo
            echo "$(rg -v -No "\+(.+?)\b" "$TODOFILE" | wc -l) with no project" ;;
        cl|contextless) todoslist | rg -v -No "@(.+?)\b" ;;
        pl|projectless) todoslist | rg -v -No "\+(.+?)\b" ;;
        rm) todo_rm "$1" ;;
        do) todo_do "$1" ;;
        undo) todo_undo "$1" ;;
        done) cat "$DONEFILE" | rg "^-" | cut -c3- | cat -n ;;
        cleardone) sed 'd' "$DONEFILE" > "$DONEFILE" ;;
        *) echo "$_usage" ;;
    esac
}
function print_todo_count {
    echo "Tasks: $(cat "$TODOFILE" | wc -l)"
}

function todoslist {
    [ -z "$TODOFILE" ] && echo "TODOFILE not defined" && return 1
    if [ -z "$@" ]; then
        cat "$TODOFILE" | rg "^\-" | cut -c3- | cat -n
    else
        cat "$TODOFILE" | rg "^\-" | cut -c3- | cat -n | rg "$@" --color=never
    fi
}

function todo_rm {
    if [ -z "$1" ]; then
        echo "Must pass an index to 'rm'"
        return 1
    fi
    echo "REMOVING $(sed -n "$1p" $TODOFILE)"
    if [ $1 -le 0 ]; then
        echo "Argument must be greater than 0"
        echo "  1-indexed line number of task (from 't ls')"
        return 2
    fi
    sed "$1d" "$TODOFILE" > $TODOFILE.tmp
    mv $TODOFILE.tmp $TODOFILE
    # -s checks if file has 'something'
    # e.g. exists and is not empty
    if [ ! -s $TODOFILE ]; then
        echo "TODOFILE now empty."
        echo "If unexpected, revert using dropbox history"
        echo "or $TODOFILE.bak"
    fi
}

function todo_do {
    if [ -z "$1" ]; then
        echo "Must pass an index to 'do'"
        return 1
    fi
    echo -n "COMPLETING $(sed -n "$1p" $TODOFILE)"
    msg="$(sed -n "$1p" $TODOFILE) done:$(date +%F)"
    echo "$msg" >> $DONEFILE
    sed "$1d" "$TODOFILE" > $TODOFILE.tmp
    mv $TODOFILE.tmp $TODOFILE
    # -s checks if file has 'something'
    # e.g. exists and is not empty
    if [ ! -s $TODOFILE ]; then
        echo "TODOFILE now empty."
        echo "If unexpected, revert using dropbox history"
        echo "or $TODOFILE.bak"
    fi
}

function todo_undo {
    if [ -z "$1" ]; then
        echo "Must pass an index to 'undo'"
        return 4
    fi
    if [ $1 -le 0 ]; then
        echo "Argument must be greater than 0"
        echo "  1-indexed line number of task (from 't ls')"
        return 3
    fi
    sed -n "$1p" "$DONEFILE" >> "$TODOFILE"
    # -s checks if file has 'something'
    # e.g. exists and is not empty
    sed "$1d" "$DONEFILE" > "$DONEFILE".tmp
    mv "$DONEFILE.tmp" "$DONEFILE"
}

main "$@"
