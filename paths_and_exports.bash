export TERM=xterm-256color
export EDITOR="nvim"
export GOPATH="$HOME/go"
export GOBIN="$HOME/bin"
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export FZF_ALT_C_COMMAND='fd -t d . $HOME'
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export DATADIR="$HOME/Dropbox/data"
export CODEDIR="$HOME/code"
export TODOFILE="$HOME/Dropbox/notes/todo.md"
export FINANCEFILE="$DATADIR/finances.csv"
export NOTESDIR="$HOME/Dropbox/notes"
export NOTESBACKUPDIR="$CODEDIR/knowledge"
export SHELLFUNCS="$CODEDIR/dotfiles/functions.zsh"

# =====================
#         Paths
# =====================
export PATH=$HOME/.vim/bundle/fzf/bin:$PATH;
export PATH=/usr/local/lib/node_modules:$PATH;
export PATH=$GOBIN:$PATH;
export PATH=$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin:$PATH;
export PATH=/Users/davison/Library/Python/3.7/bin/:$PATH;
export PATH=/Applications/Julia-1.0.app/Contents/Resources/julia/bin/:$PATH;
export PATH=$CODEDIR/scripts/:$PATH;
export PATH=$HOME/.cargo/bin/:$PATH;
export PATH=$HOME/bin:$PATH;
export PATH=$HOME/.virtualenvs/:$PATH;
export PATH=/usr/local/miniconda3/bin:$PATH;

