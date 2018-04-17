#!/bin/bash

dirty=`git status --porcelain -uno | sed s/^...//`
last_modified=`git show --pretty="format:" --name-only HEAD`

if [ -n "$dirty" ]; then
    $EDITOR $dirty
else
    $EDITOR $last_modified
fi
