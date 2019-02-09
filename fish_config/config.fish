set -U TERM xterm-256color
set -U EDITOR nvim
set -U GOPATH $HOME
set -U GOBIN $GOPATH/bin
set -U FZF_DEFAULT_COMMAND rg --files --no-ignore --hidden --follow --glob "!.git/*"
set -U WORKON_HOME $HOME/.envs
set -U LESS FRSX
set -U CODEDIR $HOME/code
set -U TODOFILE $HOME/Dropbox/notes/todo.md
set -U DONEFILE $HOME/Dropbox/notes/done.md
set -U ASMRFILE $HOME/Dropbox/asmr.csv
set -U NOTESDIR $HOME/Dropbox/notes
set -U NOTESBACKUPDIR $CODEDIR/knowledge

# =====================
#         Paths
# =====================
set PATH $HOME/.vim/bundle/fzf/bin $PATH
set PATH /usr/local/lib/node_modules $PATH
set PATH $GOBIN $PATH
set PATH $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin $PATH
set PATH /Users/davison/Library/Python/3.7/bin/ $PATH
set PATH /Applications/Julia-1.0.app/Contents/Resources/julia/bin/ $PATH
set PATH $CODEDIR/scripts/ $PATH
set PATH $HOME/.cargo/bin/ $PATH
set PATH $HOME/bin $PATH
set PATH $HOME/.virtualenvs/ $PATH
set PATH /usr/local/miniconda3/bin $PATH

alias rg="rg -S"
alias less="less -R"
alias ipython="command ipython --pprint --no-banner"
