#!/bin/bash

if [ $# -eq 0 ]
then
    echo "usage: `basename $0` <file>..."
    exit
fi
# find duplicate words
grep -Eo '(\b.+) \1\b' $1 || true
