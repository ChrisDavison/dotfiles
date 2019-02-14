set -x TERM xterm-256color
set -x EDITOR nvim
set -x GOPATH $HOME
set -x GOBIN $GOPATH/bin
set -gx FZF_DEFAULT_COMMAND 'fd --type f --hidden --follow --exclude .git'
set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
set -x WORKON_HOME $HOME/.envs
set -x LESS FRSX
set -x CODEDIR $HOME/code
set -x TODOFILE $HOME/Dropbox/notes/todo.md
set -x DONEFILE $HOME/Dropbox/notes/done.md
set -x ASMRFILE $HOME/Dropbox/asmr.csv
set -x NOTESDIR $HOME/Dropbox/notes
set -x NOTESBACKUPDIR $CODEDIR/knowledge

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
alias ls="exa --group-directories-first"
alias ll="exa --group-directories-first --long"
alias la="exa --group-directories-first --long -a"
alias lt="exa --tree -L 2"
alias lg="exa --group-directories-first --long --git-ignore"
alias ltg="exa --tree -L 2 --git-ignore"
alias g="git"
alias v="nvim"

source ~/.cargo/env
# source /usr/local/bin/virtualenvwrapper.fish
