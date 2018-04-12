#! /usr/bin/env bash

# Make find easier to use
myfind() {
    find ./ -iname "*$2*" -type "$1"
}

# And even simpler, for files and directories
ff() { myfind "f" "$1"; }
fd() { myfind "d" "$1"; }

