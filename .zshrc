export EDITOR="nvim"
export GOPATH="$HOME"
export GOBIN="$HOME/bin"
export FZF_DEFAULT_COMMAND='rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
export FZF_ALT_C_COMMAND='fd -t d . $HOME'
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export CODEDIR="$HOME/code/"
export NOTESDIR="$HOME/Dropbox/notes"
export FINANCES="$HOME/Dropbox/budget"
export VIRTUAL_ENV_DISABLE_PROMPT=1
export TODOFILE='~/Dropbox/notes/todo/todo.txt'
export DONEFILE='~/Dropbox/notes/todo/done.txt'


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
if [ -x $(which hub) ]; then
    alias g="hub"
fi
alias today="date +%F"
alias tmux="tmux -2"
alias ts="tagsearch"
alias bm="bookmarks"
alias b="bat --map-syntax txt:markdown --style=grid,header"
alias n="echo '-  $argv' >> ~/Dropbox/notes/inbox.txt"
alias nt="echo '-  [ ]$argv' >> ~/Dropbox/notes/inbox.txt"
alias inbox="nvim ~/Dropbox/notes/inbox.txt"
alias t="todo.sh -a -f -d $HOME/.todo/config"
alias habits="todo.sh -a -f -d $HOME/.todo/config ls +habit | recur_filter.py"

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


[ -d $GOBIN ] && export PATH=$GOBIN:$PATH
[ -d $HOME/bin ] && export PATH=$HOME/bin:$PATH
[ -d $HOME/code/scripts/ ] && export PATH=$HOME/code/scripts/:$PATH
[ -d $HOME/.cargo/bin ] && export PATH=$HOME/.cargo/bin:$PATH
[ -d /usr/local/go/bin ] && export PATH=/usr/local/go/bin:$PATH
[ -d /snap/bin ] && export PATH=/snap/bin:$PATH

[ -d $HOME/.local/bin ] && export PATH=$HOME/.local/bin:$PATH
[ -f $HOME/.cargo/env ] && source $HOME/.cargo/env
[ -x rvm ] && rvm default
[ -f ~/code/todo.txt/todo_completion ] &&  source ~/code/todo.txt/todo_completion
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

  # Set Spaceship ZSH as a prompt
  autoload -U promptinit; promptinit
  prompt spaceship
