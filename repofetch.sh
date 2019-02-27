#!/usr/bin/env bash
set -o errexit  # Exit early on error
set -o pipefail # Exit when a pipe fails
set -o nounset  # Exit when trying to use undeclared variables

# set -o xtrace # Trace what gets executed...for debugging

# Set magic variables for current file & dir
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
__file="${__dir}/$(basename "${BASH_SOURCE[0]}")"
__base="$(basename ${__file} .sh)"
__root="$(cd "$(dirname "${__dir}")" && pwd)" # <-- change this as it depends on your app

repoloc="${HOME}code"
for dir in ${repoloc}/*; do
    pushd $dir 2>&1 > /dev/null
    pwd
    git fetch --all
    popd 2>&1 > /dev/null
done
