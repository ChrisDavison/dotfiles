function todobackup
    cp $NOTESDIR/todo.md $NOTESBACKUPDIR/todo.md
    cp $NOTESDIR/done.md $NOTESBACKUPDIR/done.md
    pushd $NOTESBACKUPDIR
    set datetime (date +"%Y-%m-%dT%H%M%S")
    echo "TODO backup "$date
    git add .
    git commit -m "TODO backup "$date
    git push
    popd
end
