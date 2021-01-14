#!/usr/bin/env bash
[[ $# -eq 0 ]] && echo "usage: datezipdir <directory>" && return
dirname=$(basename $1)
zipname=$(date +"$dirname--%Y-%m-%d.zip")
echo $zipname
zip -r $zipname $1

