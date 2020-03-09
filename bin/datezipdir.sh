#!/usr/bin/env bash
set -e
dirname=$(basename $1)
shift
zipname=$(date +"$dirname--%Y-%m-%d.zip")
echo $zipname
zip -r $zipname $@
