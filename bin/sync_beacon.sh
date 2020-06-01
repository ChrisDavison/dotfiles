#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

pushd $HOME/Dropbox/work/work-data/beacon && rsync -rv beacon@81.98.224.48:beacon.csv . && popd
