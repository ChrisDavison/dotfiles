#!/bin/bash

# change to read from file
firsts="i|we|me|us|my|mine|ours"
seconds="you|your|yours"

if [ $# -eq 0 ]
then
    echo "usage: `basename $0` <file>..."
    exit
fi

egrep -i -n --color "\\b($firsts)\\b" $*

egrep -i -n --color "\\b($seconds)\\b" $*

exit $?
