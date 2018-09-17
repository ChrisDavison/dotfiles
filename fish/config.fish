set -x -g TERM "xterm-256color"

set --export SHELL /usr/local/bin/fish
set --export EDITOR "vi"
set -e fish_greeting

set --export GOPATH $HOME
set --export GOBIN $HOME/bin

set PATH ~/.fzf/bin $PATH
set PATH /usr/local/lib/node_modules $PATH
set PATH $GOBIN $PATH
set PATH $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin $PATH
set PATH /Users/davison/Library/Python/3.7/bin/ $PATH
set PATH /Applications/Julia-1.0.app/Contents/Resources/julia/bin/ $PATH

if [ -f /usr/local/share/autojump/autojump.fish ]
    source /usr/local/share/autojump/autojump.fish
end

eval (pipenv --completion)

alias tma=choose_tmux_session

function rg
    command rg -S $argv
end

function vi
    command mvim -v $argv
end

function g
    command git $argv
end

set --export LOGBOOK_DIR $HOME/src/github.com/chrisdavison/logbook
set --export RESEARCHFIGURES $HOME/Dropbox/work/figures

fish_vi_key_bindings
