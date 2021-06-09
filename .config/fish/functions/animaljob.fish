#!/usr/bin/env fish

function animaljob
    set -l basedir /usr/local/share/wordlists
    randomWordChoice $basedir/animals.txt $basedir/jobs.txt
end
