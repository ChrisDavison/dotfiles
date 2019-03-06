#!/usr/bin/env bash
set -o errexit  # Exit early on error
set -o pipefail # Exit when a pipe fails
set -o nounset  # Exit when trying to use undeclared variables

repoloc="${HOME}/code"
echo "Fetching repos in ${repoloc}";
for dir in ${repoloc}/*; do
    pushd $dir 2>&1 > /dev/null
    git fetch -q --all &
    popd 2>&1 > /dev/null
done
