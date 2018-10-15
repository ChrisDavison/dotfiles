set -x -g TERM "xterm-256color"

set --export SHELL /usr/local/bin/fish
set --export EDITOR "vi"
set -e fish_greeting

set --export GOPATH $HOME
set --export GOBIN $HOME/bin
set --export CAPTUREFILE $HOME/Dropbox/.capture

set PATH ~/.vim/bundle/fzf/bin $PATH
set PATH /usr/local/lib/node_modules $PATH
set PATH $GOBIN $PATH
set PATH $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin $PATH
set PATH /Users/davison/Library/Python/3.7/bin/ $PATH
set PATH /Users/davison/.cargo/bin $PATH

if [ -f /usr/local/share/autojump/autojump.fish ]
    source /usr/local/share/autojump/autojump.fish
end

eval (pipenv --completion)

alias tma=choose_tmux_session
if test -x (which exa)
    # 'a' variants do not use git ignores    
    alias ls="exa --group-directories-first --git-ignore"
    alias lsa="exa --group-directories-first"
    alias ll="exa --group-directories-first --long --git-ignore"
    alias lla="exa --group-directories-first --long"
    alias la="exa --group-directories-first -a --long"
    alias lt="exa --tree -L 2 --git-ignore"
    alias lta="exa --tree -L 2"
    alias llt="exa --group-directories-first --long --tree --git-ignore"
    alias llta="exa --group-directories-first --long --tree"
end

function rg
    command rg -S $argv
end

function vi
    command mvim -v $argv
end

function g
    command git $argv
end

function capture
    set -l d (date +"%F %T")
    if test -f "$CAPTUREFILE"]
        echo "- $d $argv" >> "$CAPTUREFILE"
    else
        echo "CAPTUREFILE not defined"
    end

end

set --export LOGBOOK_DIR $HOME/src/github.com/chrisdavison/logbook
set --export RESEARCHFIGURES $HOME/Dropbox/work/figures

bind \e\[1\;5C forward-word
bind \e\[1\;5D backward-word
