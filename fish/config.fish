set -x -g TERM "xterm-256color"

set -e fish_greeting
set --export SHELL /usr/local/bin/fish
set --export EDITOR "vi"
set --export GOPATH $HOME
set --export GOBIN $HOME/bin
set --export CAPTUREFILE $HOME/Dropbox/.capture
set --export LOGBOOK_DIR $HOME/src/github.com/chrisdavison/logbook
set --export RESEARCHFIGURES $HOME/Dropbox/work/figures

set PATH ~/.vim/bundle/fzf/bin $PATH
set PATH /usr/local/lib/node_modules $PATH
set PATH $GOBIN $PATH
set PATH $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin $PATH
set PATH /Users/davison/Library/Python/3.7/bin/ $PATH
set PATH /Users/davison/.cargo/bin $PATH

if [ -f /usr/local/share/autojump/autojump.fish ]
    source /usr/local/share/autojump/autojump.fish
end

if test -x pipenv
    eval (pipenv --completion)
end

bind \e\[1\;5C forward-word
bind \e\[1\;5D backward-word
