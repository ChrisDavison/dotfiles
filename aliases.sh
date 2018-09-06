if [ -x "$(command -v gls)" ]; then
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

alias c='clear'
alias datasci="source ~/.envs/datasci/bin/activate"     # Source my common python environment
alias cl='catless'
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias nbx="jupyter nbconvert --execute --to notebook"
alias g='git'
alias dr='gr status | grep -E "behind|ahead|modified"'
alias gitsync='gr git fetch --all'
alias gitdown='gr git pull --rebase'
alias git-root='cd $(git rev-parse --show-toplevel)'
alias datetime="date +'%F %T'"
alias vi='mvim -v'
