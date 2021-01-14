#!/usr/bin/env bash
[[ $# -eq 0 ]] && echo "usage: sanitise <filename>" && return
direc=$(dirname $1)
base=$(basename $1)
echo "$base" |tr '[:upper:]' '[:lower:]' | sed 's/[^a-zA-Z0-9.-]/-/g' | tr -s - - | sed 's/\-$//g'

