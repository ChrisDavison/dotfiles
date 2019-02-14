function notezip
    pushd $NOTESBACKUPDIR
    set dt (date +"%Y%m%dT%H%M")
    git archive -o $HOME/notes-backup--$dt.zip @
    echo "Backed up. Zip: "$HOME/notes-backup--$dt.zip
    popd
end
