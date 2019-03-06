#!/usr/bin/env bash
set -o errexit  # Exit early on error
set -o pipefail # Exit when a pipe fails
set -o nounset  # Exit when trying to use undeclared variables

repoloc="${HOME}/code"
for dir in ${repoloc}/*; do
    pushd $dir 2>&1 > /dev/null
    tidy=$(basename $(pwd))
    gstat=$(git branchstat)
    if [ -n "$gstat" ]; then
        echo "$tidy:$gstat" >> ~/.repostat_all
    fi
    popd 2>&1 > /dev/null
done

cat ~/.repostat_all | column -s':' -t
rm ~/.repostat_all
