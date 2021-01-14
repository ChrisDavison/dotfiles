#!/usr/bin/env bash
[[ $# = 0 ]] && echo "usage: aesenc <file>" && return
gpg --symmetric -a --cipher-algo aes256 --output "$1".asc "$1"
echo "$1.asc created"

