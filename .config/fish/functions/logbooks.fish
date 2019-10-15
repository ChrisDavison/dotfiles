function logbooks
    fd (date +%Y) ~/Dropbox/notes/logbook -e md -x basename {} | sort | tail -n 10
end
