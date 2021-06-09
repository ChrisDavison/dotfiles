#!/usr/bin/env fish

function animalhash
    set -l basedir /usr/local/share/wordlists
    randomWordChoice -t -s "" $basedir/adjectives.txt $basedir/colours.txt $basedir/animals.txt
end
