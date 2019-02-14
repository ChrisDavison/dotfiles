function notebackup
    set -l verbose 0
    if [ (count $argv) -gt 0 ] && [ $argv[1] = "-v" ]
        set -l verbose 1
    end
    if [ ! -d $NOTESDIR ]
        echo "NOTESDIR and NOTESBACKUPDIR must both be defined"
        echo "NOTESDIR: "$NOTESDIR
        echo "NOTESBACKUPDIR: "$NOTESBACKUPDIR
    end
    set dt (date +"%Y%m%dT%H%M")
    [ $verbose -eq 1 ]; and echo $dt": "$NOTESDIR" Backup"
    [ $verbose -eq 1 ]; and echo "Removing all files"
    rm -rf $NOTESBACKUPDIR/*
    if type -q rsync
        rsync -az $NOTESDIR/* $NOTESBACKUPDIR
    else
        cp -r $NOTESDIR/* $NOTESBACKUPDIR/
    end
    cd $NOTESBACKUPDIR
    git add . > /dev/null
    git commit -m "Backup "$dt
    git push
    git archive -o $HOME/notes-backup--$dt.zip @
    [ $verbose -eq 1 ]; and echo "Backed up. Zip: "$HOME/notes-backup--$dt.zip
end
