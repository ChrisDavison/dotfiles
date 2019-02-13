function notebackup
    if [ ! -d $NOTESDIR ]
        echo "NOTESDIR and NOTESBACKUPDIR must both be defined"
        echo "NOTESDIR: "$NOTESDIR
        echo "NOTESBACKUPDIR: "$NOTESBACKUPDIR
    end
    set dt (date +"%Y%m%dT%H%M")
    echo $dt": "$NOTESDIR" Backup"
    rm -rf $NOTESBACKUPDIR/*
    if type -q rsync
        rsync -avz $NOTESDIR/* $NOTESBACKUPDIR
    else
        cp -r $NOTESDIR/* $NOTESBACKUPDIR/
    end
    cd $NOTESBACKUPDIR
    git add . > /dev/null
    git commit -m "Backup "$dt
    git push
    git archive -o $HOME/notes-backup--$dt.zip @
end
