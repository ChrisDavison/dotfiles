#!/usr/bin/env sh
case "$1" in 
    l|last) [ -f ~/.lastuuid ] && cat ~/.lastuuid ;;
    *) uuid4 | tee ~/.lastuuid;;
esac 