if [ -x "$(command -v exa)" ]; then
    alias ls="exa --group-directories-first"
    alias ll="exa --group-directories-first --long"
    alias la="exa --group-directories-first -a --long"
    alias lt="exa --tree -L 2"
elif [ -x "$(command -v gls)" ]; then
    alias ll='gls -lFh --group-directories-first --color=auto'
    alias la='gls -AlFh --group-directories-first --color=auto'
    alias ls='gls -CF --group-directories-first --color=auto'
    alias l='gls -CF --group-directories-first --color=auto'
else
    alias ll='ls -GlFh'
    alias la='ls -GAlFh'
    alias ls='ls -GCF'
    alias l='ls -GCF'
fi

alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias g='git'
alias git-root='cd $(git rev-parse --show-toplevel)'
alias vi='mvim -v'

alias tma=choose_tmux_session
alias ipython="ipython --pprint --no-banner"

