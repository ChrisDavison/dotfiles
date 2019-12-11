set -Ux fish_greeting ""

set -Ux EDITOR "nvim"
# set -Ux TERM xterm-256color
set -Ux EDITOR "nvim"
set -Ux GOPATH "$HOME"
set -Ux GOBIN "$HOME/bin"
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

alias tmux="set TERM xterm-256color; tmux"
alias c="clear"
alias cp="cp -rv"    # Always recursively and verbosely copy
alias mv="mv -v"     # Always explain move actions
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias nv="nvim.appimage"
alias ipython="ipython --pprint --no-banner"
alias g="git"
alias today="date +%F"
alias tmux="tmux -2"
alias envml="source $HOME/.envs/ml/bin/activate"
alias b="budget $FINANCES"
alias ts="tagsearch"
alias ob="openBookmarks"
alias obu="openBookmarkURL"

set myRepos "$HOME/src/github.com/ChrisDavison"
set workRepos "$HOME/src/github.com/cidcom"
if test -d $myRepos; and test -d $workRepos
    set repos "$myRepos $workRepos"
else if test -d $myRepos
    set repos "$myRepos"
else if test -d $workRepos
    set repos "$workRepos"
end
alias rf="repoutil fetch $repos"
alias rs="repoutil stat $repos"
alias rl="repoutil list $repos"
alias ru="repoutil unclean $repos"
set -e repos

alias ls="exa --group-directories-first"
alias lsi="exa --group-directories-first --git-ignore"
alias ll="ls --long --group-directories-first"
alias la="ll -a --group-directories-first"
alias lt="exa --tree -L 2 --group-directories-first"
alias lg="ll --git-ignore"
alias ltg="lt --git-ignore"

test -f $HOME/.cargo/env; and source $HOME/.cargo/env
test -x rvm; and rvm default

# opam configuration
source /home/davison/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
if test -f /usr/share/autojump/autojump.fish
    source /usr/share/autojump/autojump.fish
end

if test -f /Users/davison/.autojump/share/autojump/autojump.fish
    source /Users/davison/.autojump/share/autojump/autojump.fish
end
