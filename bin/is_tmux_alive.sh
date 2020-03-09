#!/bin/bash

set -e

if [ $(tmux list-sessions | wc -l) -gt 0 ];
then
    echo "TMUX"
else
    echo "noMUX"
fi
