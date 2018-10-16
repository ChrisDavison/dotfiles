set -x -g TERM "xterm-256color"

set -e fish_greeting
set --export SHELL /usr/local/bin/fish
set --export EDITOR "vi"
set --export GOPATH $HOME
set --export GOBIN $HOME/bin
set --export CAPTUREFILE $HOME/Dropbox/.capture
set --export LOGBOOK_DIR $HOME/src/github.com/chrisdavison/logbook
set --export RESEARCHFIGURES $HOME/Dropbox/work/figures

test -d $HOME/.vim/bundle/fzf/bin; and set PATH $HOME/.vim/bundle/fzf/bin $PATH
test -d /usr/local/lib/node_modules; and set PATH /usr/local/lib/node_modules $PATH
test -d $GOBIN; and set PATH $GOBIN $PATH
test -d $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin; and set PATH $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin $PATH
test -d $HOME/Library/Python/3.7/bin/; and set PATH $HOME/Library/Python/3.7/bin/ $PATH
test -d $HOME/.cargo/bin; and set PATH $HOME/.cargo/bin $PATH
test -d $HOME/bin; and set PATH $HOME/bin $PATH

if [ -f /usr/local/share/autojump/autojump.fish ]
    source /usr/local/share/autojump/autojump.fish
end

if test -x pipenv
    eval (pipenv --completion)
end

bind \e\[1\;5C forward-word
bind \e\[1\;5D backward-word
