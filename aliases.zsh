if [ -x "$(command -v exa)" ]; then
    alias ls="exa --group-directories-first"
    alias ll="ls --long"
    alias la="ll -a"
    alias lt="exa --tree -L 2"
    alias lg="ll --git-ignore"
    alias ltg="lt --git-ignore"
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
alias git='hub'
alias g='hub'
alias git-root='cd $(hub rev-parse --show-toplevel)'
alias vi='mvim -v'

alias tma=choose_tmux_session
alias ipython="ipython --pprint --no-banner"
