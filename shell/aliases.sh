# Aliases for bash, zsh etc

# Directory listing aliases

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

# Session management
alias c='clear'
alias ..='cd ..'

# Copy SSH public key
alias cbssh="cbf ~/.ssh/id_rsa.pub"

# Python machine-learning environment
alias datasci="source ~/.envs/datasci/bin/activate"

# Utilities
alias cl='catless'

# Make ripgrep Smart-case search by default
alias rg='rg -S'

alias ME='cd ~/src/github.com/chrisdavison'
alias CHURN='cd ~/src/github.com/etsteam/churning'

alias nbx="jupyter nbconvert --execute --to notebook"

# Docker bollocks
alias batrem='pmset -g batt | rg -o "\d+:\d+ remaining"'

