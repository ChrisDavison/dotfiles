#!/usr/bin/env bash

export DISPLAY=$(grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0
export LIBGL_ALWAYS_INDIRECT=1
export NO_AT_BRIDGE=1
[ -f ~/.wsl_interop ] && rm -f ~/.wsl_interop
for i in $(pstree -np -s $fish_pid | grep -o -E '[0-9]+'); do
    fname="/run/WSL/$i"_interop
    if [ -f "$fname" ]; then
        export WSL_INTEROP=$fname
        echo $fname > ~/.wsl_interop
    fi
done

