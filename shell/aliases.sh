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
alias grst='gr status'
alias grstv='gr status -v'
alias grl="gr git --no-pager log --decorate --graph --oneline -n 3"

alias dr='gr status | grep -E "behind|ahead|modified"'
alias gitsync='gr git fetch --all'
alias gitdown='gr git pull --rebase'

# Utilities
alias cl='catless'

alias mdepub="pandoc -s -t epub"

alias tma='tmux attach -d -t'
alias tmuxhere='tmux new -s $(basename $(pwd))'
alias mux="tmuxinator"

nbconvclean() {
    fn="${1}"
    noIn=--"${2}"Exporter.exclude_input=True
    noRaw=--"${2}"Exporter.exclude_raw=True
    fmtLow=`echo $2 | tr /A-Z/ /a-z/`
    jupyter nbconvert --to="$fmtLow" "$noIn" "$noRaw" $1
}
# Make ripgrep Smart-case search by default
alias rg='rg -S'

alias ME='cd ~/src/github.com/chrisdavison'

alias nbx="jupyter nbconvert --execute --to notebook"