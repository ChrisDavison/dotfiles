set -U TERM xterm-256color
set -U EDITOR code -w
set -U GOPATH $HOME/go
set -U GOBIN $HOME/bin
set -U FZF_DEFAULT_COMMAND rg --files --no-ignore --hidden --follow --glob "!.git/*"
set -U FZF_ALT_C_COMMAND fd -t d . $HOME
set -U WORKON_HOME $HOME/.envs
set -U LESS FRSX
set -U DATADIR $HOME/Dropbox
set -U CODEDIR $HOME/code
set -U TODOFILE $HOME/Dropbox/todo.md
set -U NOTESDIR $HOME/Dropbox
set -U fish_greeting ""

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
set PATH /usr/local/sbin $PATH

alias rg="rg -S"
alias less="less -R"
alias ipython="command ipython --pprint --no-banner"
alias ls="exa --group-directories-first"
alias ll="ls --long"
alias la="ll -a"
alias lt="exa --tree -L 2"
alias lg="ll --git-ignore"
alias ltg="lt --git-ignore"
alias c=clear
alias v=nvim
alias rf="repofetch"
alias rs="reposstat"
alias a="asmr.py"
alias t="$EDITOR $TODOFILE"
alias tt="cat -n $TODOFILE"

# Git aliases
alias g="git"
# alias ga="git add"
# alias gaa="git add --all"
# alias gap="git add --patch"
# alias gc="git commit -v"
# alias gc!="git commit -v --amend"
# alias gca="git commit -v -a"
# alias gcl="git config --list"
# alias gss="git stash show --text"
# alias gsc="git stash clear"
# alias gwch="git whatchanged -p --abbrev-commit --pretty=medium"
# alias gp="git pull"
