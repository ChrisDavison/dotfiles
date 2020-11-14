#!/usr/bin/env zsh

# Windows / WSL-specific config
if [[ $(uname -a | grep -i -q 'Microsoft') -eq 0 ]]; then
    export USING_WSL=1
fi

setup_wsl_interop(){
    if [[ "$USING_WSL" -eq 1 ]]; then
        export BROWSER=$(which firefox)
        export DISPLAY=$(grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0
        export LIBGL_ALWAYS_INDIRECT=1
        export NO_AT_BRIDGE=1
        for i in $(pstree -np -s $$ | grep -o -E '[0-9]+'); do
            fname=/run/WSL/"$i"_interop
            if [[ -e "$fname" ]]; then
                export WSL_INTEROP=$fname
                [[ -f "~/.wsl_interop" ]] && rm "~/.wsl_interop"
                echo $fname > ~/.wsl_interop
            fi
        done
    fi
}

setup_wsl_interop
