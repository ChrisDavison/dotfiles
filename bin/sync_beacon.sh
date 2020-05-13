#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

pushd $HOME/Dropbox/work-data/beacon && rsync -rv beacon@192.168.0.27:beacon.csv . && popd
