function JournalNeg
	for line in (cat $NOTESDIR/journal.md | 
        sed -E "s/\*\*[0-9]{4}-[0-9]{2}-[0-9]{2}\*\* :: //" |
        tr -s ".?" \n | 
        sed "s/^ *//" |
        rg -F "(-)" |
        sed -e "s/ *(\-)//g")
        echo $line
    end
end
