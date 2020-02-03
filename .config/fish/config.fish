set -Ux fish_greeting ""

set -Ux EDITOR "nvim"
# set -Ux TERM xterm-256color
set -Ux EDITOR "nvim"
set -Ux GOPATH "$HOME"
set -Ux GOBIN "$HOME/.bin"
set -Ux FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
set -Ux FZF_ALT_C_COMMAND 'fd -t d . $HOME'
set -Ux WORKON_HOME "$HOME/.envs"
set -Ux LESS FRSX
set -Ux CODEDIR "$HOME/src/github.com/"
set -Ux NOTESDIR "$HOME/Dropbox/notes"
set -Ux FINANCES "$HOME/Dropbox/budget"

set PATH $GOBIN $PATH
set PATH $HOME/.bin $PATH
set PATH $CODEDIR/ChrisDavison/scripts/ $PATH
set PATH $HOME/.cargo/bin $PATH
set PATH /usr/local/go/bin $PATH
set PATH /snap/bin $PATH

alias tmux="set TERM xterm-256color; tmux"
alias c="clear"
alias cp="cp -rv"    # Always recursively and verbosely copy
alias mv="mv -v"     # Always explain move actions
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
alias g="git"
alias today="date +%F"
alias tmux="tmux -2"
alias envml="source $HOME/.envs/ml/bin/activate"
alias ts="tagsearch"
alias ob="openBookmarks"
alias obu="openBookmarkURL"
alias b="bat --map-syntax txt:markdown --style=grid,header"
alias n="echo '-  $argv' >> ~/Dropbox/notes/inbox.txt"
alias nt="echo '-  [ ]$argv' >> ~/Dropbox/notes/inbox.txt"
alias inbox="nvim ~/Dropbox/notes/inbox.txt"

alias ru="repoutil unclean"
alias rs="repoutil stat"
alias rl="repoutil list"
alias rf="repoutil fetch"

alias ls="exa --group-directories-first"
alias lsi="exa --group-directories-first --git-ignore"
alias ll="ls --long --group-directories-first"
alias la="ll -a --group-directories-first"
alias lt="exa --tree -L 2 --group-directories-first"
alias lg="ll --git-ignore"
alias ltg="lt --git-ignore"

test -f $HOME/.cargo/env; and source $HOME/.cargo/env
test -x rvm; and rvm default

status --is-interactive; and cd $HOME/Dropbox/notes
