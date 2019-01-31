_usage="usage: t <CMD> [ARGS...]

Generally, assume QUERY is literal (e.g. rg -F QUERY).

Modifying:
    a|add TEXT...    Add a task
    rm #NUM          Remove item #NUM
    do #NUM          Mark item #NUM as done
    undo #NUM        Move item #NUM from DONEFILE into TODOFILE
    up #NUM          Upgrade task #NUM to a priority
    down #NUM        Downgrade task #NUM to a normal task
    addp PROJECT     Add project to the end of a note (e.g. +PROJECT)
    addc CONTEXT     Add context to the end of a note (e.g. +CONTEXT)

Viewing:
    ls [QUERY]       List tasks (optionally filtered)
    lsp [QUERY]      List prioritised tasks (optionally filtered)
    hide QUERY       List notes NOT matching query
    c|contexts       List all unique contexts '+CONTEXT'
    p|projects       List all unique projects '@PROJECT'

Filtered views:
    cl|contextless            Tasks without a context
    pl|projectless            Tasks without a project
    done                      List done tasks
    filters                   View available filters
    filter FILTER             Filter using a filter available.  See filters command"

function todo_append {
    num=$1
    shift
    [ -z "$num" ] && echo "Must pass >0 #NUM to 'append'" && return 1
    [ $num -le 0 ] && echo "Must pass >0 #NUM to 'append'" && return 2
    sed -i.bak "$num s/$/ $@/" "$TODOFILE"
}

function todo_add_proj {
    num=$1
    shift
    [ -z "$num" ] && echo "Must pass >0 #NUM to 'addp'" && return 1
    [ $num -le 0 ] && echo "Must pass >0 #NUM to 'addp'" && return 2
    sed -i.bak "$num s/$/ +$@/" "$TODOFILE"
}

function todo_add_proj {
    num=$1
    shift
    [ -z "$num" ] && echo "Must pass >0 #NUM to 'addc'" && return 1
    [ $num -le 0 ] && echo "Must pass >0 #NUM to 'addc'" && return 2
    sed -i.bak "$num s/$/ @$@/" "$TODOFILE"
}

function todo_upgrade {
    [ -z "$@" ] && echo "Must pass QUERY to 'up'" && return 1
    sed -i.bak "$@s/- /- ! /" "$TODOFILE"
}

function todo_downgrade {
    [ -z "$@" ] && echo "Must pass QUERY to 'down'" && return 1
    sed -i.bak "$@s/- ! /- /" "$TODOFILE" > "$TODOFILE.bak"
}

function todo_list {
    query=$@
    cat $HOME/.todos | rg "\d\s+! " | rg -F "$query" --color=never
    cat $HOME/.todos | rg -v "\d\s+! " | rg -F "$query" --color=never
}

function todo_hide {
    query=$@
    cat $HOME/.todos | rg "\d\s+! " | rg -v -F "$query" --color=never
    cat $HOME/.todos | rg -v "\d\s+! " | rg -v -F "$query" --color=never
}

function todo_rm {
    [ -z "$1" ] && echo "Must pass an index to 'rm'" &&  return 1
    [ $1 -le 0 ] && echo "#NUM must be >0 (line number from 't ls')" && return 2
    task=$(sed -n "$1p" $TODOFILE)
    echo "REMOVING $task"
    sed -i.bak "$1d" "$TODOFILE"
}

function todo_do {
    [ -z "$1" ] && echo "Must pass an index to 'do'" && return 1
    task=$(sed -n "$1p" $TODOFILE)
    echo -n "COMPLETING $task"
    msg="$task done:$(date +%F)"
    echo "$msg" >> $DONEFILE
    sed -i.bak "$1d" "$TODOFILE"
}

function todo_undo {
    [ -z "$1" ] && echo "Must pass an index to 'undo'" && return 1
    [ $1 -le 0 ] && echo "#NUM must be >0 (line number from 't ls')" && return 2
    sed -n "$1p" "$DONEFILE" >> "$TODOFILE"
    sed -i.bak "$1d" "$DONEFILE"
}

[ -z "$TODOFILE" ] && echo "TODOFILE not defined" && return 1
[ -z "$DONEFILE" ] && echo "DONEFILE not defined" && return 2

cat "$TODOFILE" | rg "^\-" | cut -c3- | cat -n >> $HOME/.todos

TASKCOUNT=$(cat "$TODOFILE" | wc -l)
DONECOUNT=$(cat "$DONEFILE" | wc -l)

cmd=$1
shift
case $cmd in
    a|add)
        echo "- $@" >> "$TODOFILE" ;;
    app|append)
        todo_append "$@" ;;
    addp|addproject)
        todo_add_proj "$@" ;;
    addc|addcontext)
        todo_add_context "$@" ;;
    ls)
        todo_list "$@" ;;
    lsp)
        todo_list "\d\s+!.*$@" ;;
    c|contexts)
        echo "Tasks: $TASKCOUNT"
        rg -No "@(.+?)\b" "$TODOFILE" | sort | uniq -c
        echo
        echo "$(rg -v -No "@(.+?)\b" "$TODOFILE" | wc -l) with no context" ;;
    p|projects)
        echo "Tasks: $TASKCOUNT"
        rg -No "\+(.+?)\b" "$TODOFILE" | sort | uniq -c
        echo
        echo "$(rg -v -No "\+(.+?)\b" "$TODOFILE" | wc -l) with no project" ;;
    up|upgrade)
        todo_upgrade "$@" ;;
    down|downgrade)
        todo_downgrade "$@" ;;
    cl|contextless)
        todo_list | rg -v -N "@(.+?)\b" ;;
    pl|projectless)
        todo_list | rg -v -N "\+(.+?)\b" ;;
    hide)
        todo_hide "$@" ;;
    rm)
        todo_rm "$1" ;;
    do)
        todo_do "$1" ;;
    undo)
        todo_undo "$1" ;;
    done)
        cat "$DONEFILE" | rg "^\-" | cut -c3- | cat -n ;;
    cleardone)
        sed 'd' "$DONEFILE" > "$DONEFILE" ;;
    *)
        echo "$_usage" ;;
esac

if [ $TASKCOUNT -gt 0 -a ! -s "$TODOFILE" ]; then
    echo "TODOFILE now empty. If unexpected, revert using dropbox history or $TODOFILE.bak"
    echo
    read -p "Revert to $TODOFILE.bak?" revert
    case $revert in
        y|yes|Y|YES) cp "$TODOFILE.bak" "$TODOFILE" ;;
        *) ;;
    esac
fi
if [ $DONECOUNT -gt 0 -a ! -s "$DONEFILE" ]; then
    echo "DONEFILE now empty.  If unexpected, revert using dropbox history or $DONEFILE.bak"
    echo
    read -p "Revert to $DONEFILE.bak?" revert
    case $revert in
        y|yes|Y|YES) cp "$DONEFILE.bak" "$DONEFILE" ;;
        *) ;;
    esac
fi

rm $HOME/.todos
