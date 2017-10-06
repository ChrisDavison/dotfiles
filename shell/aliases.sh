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
alias h='history'
alias j='jobs -l'
alias ..='cd ..'
alias .2="cd ../.."
alias .3="cd ../../.."
alias .4="cd ../../../.."
alias .5="cd ../../../../.."

# Copy SSH public key
alias cbssh="cbf ~/.ssh/id_rsa.pub"

# Python machine-learning environment
alias datasci="source ~/.virtualenvs/datasci/bin/activate"

# Nice git aliases
alias g='git'
alias gap='git add -p'

# Mixu git-run (gr) aliases...
# For handling multiple git repos
alias grst='gr @live status'
alias grstv='gr @live status -v'
alias grest='gr @engd status'
alias grpst='gr @personal status'
alias grl="gr @live git --no-pager log --decorate --graph --oneline -n 3"

alias dr='gr status | grep -E "behind|ahead|modified"'
alias gitsync='(cd ~/ && mu up --all)'

# Utilities
alias cl='catless'

alias mdepub="pandoc -s -t epub"

alias tma='tmux attach -d -t'
alias tmuxhere='tmux new -s $(basename $(pwd))'
alias mux="tmuxinator"
