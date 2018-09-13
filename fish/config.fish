set -x -g TERM "xterm-256color"

set --export SHELL /usr/local/bin/fish
set --export EDITOR "vi"
set -e fish_greeting

set PATH ~/.fzf/bin $PATH

if [ -f /usr/local/share/autojump/autojump.fish ]
    source /usr/local/share/autojump/autojump.fish
end

eval (pipenv --completion)

alias rg="rg -S"
alias vi="mvim -v"
alias tma=choose_tmux_session
alias g=git

set --export GOPATH $HOME
set --export GOBIN $HOME/bin
set --export TEXPATH /usr/texbin
set --export NODEPATH /usr/local/lib/node_modules
set --export HOMEBIN $GOBIN
set --export MULTIRUSTBIN $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin
set --export JULIAPATH /Applications/Julia-1.0.app/Contents/Resources/julia/bin/
set PATH $JULIAPATH $GOBIN $TEXBIN $NODEPATH $HOMEBIN $MULTIRUSTBIN $STACKBIN $PATH

set --export LOGBOOK_DIR $HOME/src/github.com/chrisdavison/logbook
set --export LOGBOOK_DIR $HOME/Dropbox/work/figures
