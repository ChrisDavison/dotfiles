#!/usr/bin/env fish

function wsl_interop_setup
    set -Ux DISPLAY (grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0
    set -Ux LIBGL_ALWAYS_INDIRECT 1

    set -Ux NO_AT_BRIDGE 1
    rm -f ~/.wsl_interop
    for i in (pstree -np -s $fish_pid | grep -o -E '[0-9]+')
        set -l fname "/run/WSL/"$i"_interop"
        if test -e $fname
            set -x WSL_INTEROP $fname
            echo $fname > ~/.wsl_interop
        end
    end
end
