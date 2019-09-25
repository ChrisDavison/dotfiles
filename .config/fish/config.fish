set -Ux fish_greeting ""

set -Ux EDITOR "nvim"
set -Ux TERM xterm-256color
set -Ux EDITOR "nvim"
set -Ux GOPATH "$HOME/go"
set -Ux GOBIN "$HOME/bin"
set -Ux FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
set -Ux FZF_ALT_C_COMMAND 'fd -t d . $HOME'
set -Ux WORKON_HOME "$HOME/.envs"
set -Ux LESS FRSX
set -Ux CODEDIR "$HOME/code"
set -Ux NOTESDIR "$HOME/Dropbox/notes"
set -Ux BUDGET_CONFIG "$HOME/Dropbox/house/income.csv"
set -Ux BUDGET_COSTS "$HOME/Dropbox/house/costs.csv"

alias c="clear"
alias cp="cp -rv"    # Always recursively and verbosely copy
alias mv="mv -v"     # Always explain move actions
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
alias rf="repoutil fetch"
alias rs="repoutil stat"
alias g="git"
alias today="date +%F"
alias tmux="tmux -2"
alias envml="source $HOME/.envs/ml/bin/activate"

alias ls="exa --group-directories-first"
alias lsi="exa --group-directories-first --git-ignore"
alias ll="ls --long"
alias la="ll -a"
alias lt="exa --tree -L 2"
alias lg="ll --git-ignore"
alias ltg="lt --git-ignore"

test -f $HOME/.cargo/env; and source $HOME/.cargo/env
