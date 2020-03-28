set -Ux fish_greeting ""

set -Ux EDITOR "nvim"
set -Ux EDITOR "nvim"
set -Ux GOPATH "$HOME"
set -Ux GOBIN "$HOME/bin"
set -Ux FZF_DEFAULT_COMMAND 'rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
set -Ux FZF_ALT_C_COMMAND 'fd -t d . $HOME'
set -Ux WORKON_HOME "$HOME/.envs"
set -Ux LESS FRSX
set -Ux CODEDIR "$HOME/code/"
set -Ux NOTESDIR "$HOME/Dropbox/notes"
set -Ux FINANCES "$HOME/Dropbox/budget"
set -Ux VIRTUAL_ENV_DISABLE_PROMPT 1
set -Ux TODOFILE ~/Dropbox/notes/todo.txt
set -Ux DONEFILE ~/Dropbox/notes/done.txt

test -d $GOBIN; and set PATH $GOBIN $PATH
test -d $HOME/bin; and set PATH $HOME/.bin $PATH
test -d $HOME/code/scripts/; and set PATH $HOME/code/scripts/ $PATH
test -d $HOME/.cargo/bin; and set PATH $HOME/.cargo/bin $PATH
test -d /usr/local/go/bin; and set PATH /usr/local/go/bin $PATH
test -d /snap/bin; and set PATH /snap/bin $PATH
test -d $HOME/.local/bin; and set PATH $HOME/.local/bin $PATH

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
test -e (which hub); and alias g="hub"
alias today="date +%F"
alias tmux="tmux -2"
alias ts="tagsearch"
alias bm="bookmarks"
alias b="bat --map-syntax txt:markdown --style=grid,header"
alias n="echo '-  $argv' >> ~/Dropbox/notes/inbox.txt"
alias nt="echo '-  [ ]$argv' >> ~/Dropbox/notes/inbox.txt"
alias inbox="nvim ~/Dropbox/notes/inbox.txt"
alias t="todo.sh -a -f"
alias habits="todo.sh -a -f due +@habit"
alias thesis="todo.sh -a -f lsp @thesis"
alias tp="todo.sh -a -f contextview @personal"
alias tm="todo.sh -a -f contextview @media"
alias n="note.py"

alias ru="repoutil unclean"
alias rs="repoutil stat"
alias rl="repoutil list"
alias rf="repoutil fetch"
alias rb="repoutil branchstat"

alias ls="exa --group-directories-first --git-ignore"
alias lsa="exa --group-directories-first"
alias ll="ls --long --group-directories-first"
alias la="ll -a --group-directories-first"
alias lt="exa --tree -L 2 --group-directories-first"
alias lg="ll --git-ignore"
alias ltg="lt --git-ignore"

test -f $HOME/.cargo/env; and source $HOME/.cargo/env
test -x rvm; and rvm default
test -f ~/code/todo.txt/todo_completion; and bash ~/code/todo.txt/todo_completion
