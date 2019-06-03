if [ -x "$(command -v exa)" ]; then
    alias ls="exa --group-directories-first"
    alias ll="ls --long"
    alias la="ll -a"
    alias lt="exa --tree -L 2"
    alias lg="ll --git-ignore"
    alias ltg="lt --git-ignore"
elif [ -x "$(command -v gls)" ]; then
    alias ll='gls -lFh --group-directories-first --color=auto'
    alias la='gls -AlFh --group-directories-first --color=auto'
    alias ls='gls -CF --group-directories-first --color=auto'
    alias l='gls -CF --group-directories-first --color=auto'
else
    alias ll='ls -GlFh'
    alias la='ls -GAlFh'
    alias ls='ls -GCF'
    alias l='ls -GCF'
fi

alias c="clear"
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias tma=choose_tmux_session
alias ipython="ipython --pprint --no-banner"
alias rf="repofetch"
alias rs="reposstat"
alias a="asmr"
alias t="${EDITOR:-vi} $TODOFILE"
alias tt="cat -n $TODOFILE"
alias j="${EDITOR:-vi} ~/Dropbox/notes/journal.md"
alias sql3="sqlite3 -header -column"

# Git aliases
alias g="git"
alias ga="git add"
alias gaa="git add --all"
alias gap="git add --patch"
alias gc="git commit -v"
alias gc!="git commit -v --amend"
alias gca="git commit -v -a"
alias gcl="git config --list"
alias gss="git stash show --text"
alias gsc="git stash clear"
alias gwch="git whatchanged -p --abbrev-commit --pretty=medium"
alias gp="git pull"
