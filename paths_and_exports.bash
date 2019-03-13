export TERM=xterm-256color
export EDITOR="nvim"
export GOPATH="$HOME"
export GOBIN="$GOPATH/bin"
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export FZF_ALT_C_COMMAND='fd -t d . $HOME'
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export DATADIR="$HOME/Dropbox/data"
export CODEDIR="$HOME/src/github.com/chrisdavison"
export TODOFILE="$HOME/Dropbox/notes/todo.md"
export DONEFILE="$HOME/Dropbox/notes/done.md"
export FINANCEFILE="$DATADIR/finances.csv"
export NOTESDIR="$HOME/Dropbox/notes"
export NOTESBACKUPDIR="$CODEDIR/knowledge"
export SHELLFUNCS="$CODEDIR/dotfiles/functions.zsh"

# =====================
#         Paths
# =====================
path+=$HOME/.vim/bundle/fzf/bin
path+=/usr/local/lib/node_modules
path+=$GOBIN
path+=$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin
path+=/Users/davison/Library/Python/3.7/bin/
path+=/Applications/Julia-1.0.app/Contents/Resources/julia/bin/
path+=$CODEDIR/scripts/
path+=$HOME/.cargo/bin/
path+=$HOME/bin
path+=$HOME/.virtualenvs/
path+=/usr/local/miniconda3/bin

# Remove duplicates from $PATH
typeset -aU path
