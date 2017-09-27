#! /usr/bin/env bash
# --- Functions for dealing with CSV files
## Print rows until a regex is found at the start of a row
print_before () {
    awk -v date=$1 '$0 ~ "^"date{exit} {print $0}' $2
}

# Ignore rows until a regex is found at the start of a row
print_after () {
    awk -v date=$1 '$0 ~ "^"date{start=1} start==1{print $0}' $2
}

# Ignore rows before regex1 is found, exit after regex2 is found.
print_between () {
    awk -v date1=$1 -v date2=$2 '$0 ~ "^"date1{start=1} $0 ~ "^"date2{exit} start==1{print $0}' $3
}