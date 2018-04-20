#! /usr/bin/env bash
epoch2date () {
    epc=$1
    fmt='+%Y%m%d %H:%M:%S'
    date -r $epc $fmt
}

lastmonday(){
    date -v -monday +"%Y%m%d"
}

timenow(){
    date +"%Y-%m-%d %H:%M:%S"
}

nextsunday(){
    $(date -v +sunday -v +7d +"%Y-%m-%d")
}
