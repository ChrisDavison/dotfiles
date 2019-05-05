set -Ux TERM xterm-256color
set -Ux EDITOR "code -w"
set -Ux GOPATH $HOME/go
set -Ux GOBIN $HOME/bin
set -Ux FZF_DEFAULT_COMMAND rg --files --no-ignore --hidden --follow --glob "!.git/*"
set -Ux FZF_ALT_C_COMMAND fd -t d . $HOME
set -Ux WORKON_HOME $HOME/.envs
set -Ux LESS FRSX
set -Ux DATADIR $HOME/Dropbox
set -Ux CODEDIR $HOME/code
set -Ux TODOFILE $HOME/Dropbox/todo.md
set -Ux NOTESDIR $HOME/Dropbox
set -Ux fish_greeting ""

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
alias lsi="exa --group-directories-first --git-ignore"
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
alias ga="git add"
alias gaa="git add --all"
alias gap="git add --patch"
alias gc="git commit -v"
alias gc!="git commit -v --amend"
alias gcne="git commit --amend --no-edit"
alias gss="git stash show --text"
alias gsc="git stash clear"
alias gsp="git stash pop"
alias gwch="git whatchanged -p --abbrev-commit --pretty=medium"
