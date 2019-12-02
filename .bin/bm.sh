#!/usr/bin/env sh
GREPPER=grep
if [ -x $(which rg) ]; then
    GREPPER=rg
fi

$GREPPER "tags:" | cut -d':' -f3- | sed -e 's/ /\n/g' | sort | uniq -c | sort -r | head -n10
