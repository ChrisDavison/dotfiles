#!/usr/bin/env bash
set -o errexit  # Exit early on error
set -o pipefail # Exit when a pipe fails
set -o nounset  # Exit when trying to use undeclared variables

loc1="${HOME}/src/github.com/chrisdavison"
loc2="${HOME}/src/github.com/cidcom"

for dir in ${loc1}/* ${loc2}/*; do
    (cd "$dir" && git sstat)
done
