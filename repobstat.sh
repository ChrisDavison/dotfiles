#!/usr/bin/env bash
set -o errexit  # Exit early on error
set -o pipefail # Exit when a pipe fails
set -o nounset  # Exit when trying to use undeclared variables

stat_in(){
    repoloc="$1"
    outname="$HOME/.repos_status"
    for dir in ${repoloc}/*; do
        pushd $dir 2>&1 > /dev/null
        tidy=$(basename $(pwd))
        gstat=$(git branchstat)
        if [ -n "$gstat" ]; then
            echo "$tidy:$gstat" >> ${outname}
        fi
        popd 2>&1 > /dev/null
    done
}

stat_in "${HOME}/src/github.com/chrisdavison"
stat_in "${HOME}/src/github.com/cidcom"

if [ -f ${outname} ]; then
    cat ${outname} | column -s':' -t
    rm ${outname}
else
    echo "No repo has changes"
fi
