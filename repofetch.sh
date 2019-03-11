#!/usr/bin/env bash
set -o errexit  # Exit early on error
set -o pipefail # Exit when a pipe fails
set -o nounset  # Exit when trying to use undeclared variables

fetch_in() {
    echo "Fetching repos in $1";
    parallel 'cd {} && git fetch -q --all' ::: "$1"/*
}

fetch_in "${HOME}/src/github.com/chrisdavison"
fetch_in "${HOME}/src/github.com/cidcom"
