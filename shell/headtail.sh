#! /usr/bin/env bash
ht () {
    if [ $# -lt 1 ]; then
        echo "Usage: ht FILE [DELIMITER=' ']"
        return -1
    fi
    if [ $# -eq 1 ]; then 
        delim=' '
    fi
    if [ $# -ge 2 ]; then 
        delim=$2
    fi
    awk -F $delim 'NR == 2 { printf $1; exit }' $1 && echo -n " till "
    tail -n 1 $1 | awk -F $delim '{ print $1 }'
  #    head -n 2 $1 && tail -n 1 $1
}