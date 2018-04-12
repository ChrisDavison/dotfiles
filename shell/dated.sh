#! /usr/bin/env bash
epoch2date () {
    epc=$1
    fmt='+%Y%m%d %H:%M:%S'
    date -r $epc $fmt
}
