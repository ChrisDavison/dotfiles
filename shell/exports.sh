# Exports for bash/zsh
export EDITOR="vim";
[ -x `which nvim` ] && export EDITOR="nvim";

# Terminal history handling
export HISTCONTROL=ignoreboth:erasedups;
export HISTSIZE=32768;
export HISTFILE=~/.zsh_history
export HISTFILESIZE=$HISTSIZE;
export HISTFILESIZE=2000;
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help";
export SAVEHIST=9000

# Environment variables/path
export GOPATH="$HOME";
export GOBIN="$HOME/bin";
export TEXPATH="/usr/texbin";
export NODEPATH="/usr/local/lib/node_modules";
export HOMEBIN="$GOBIN"
export MULTIRUSTBIN="$HOME/.multirust/toolchains/nightly/cargo/bin"
export STACKBIN="$HOME/.local/bin"
export RESEARCHFIGURES="$HOME/Dropbox/f/figures/"

export PATH=$GOBIN:$TEXBIN:$NODEPATH:$HOMEBIN:$MULTIRUSTBIN:$STACKBIN:$PATH;

# 256 color terminal
export TERM=xterm-256color;

export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3

# Export various directories useful for mu work
export CODE="$HOME/src/github.com/chrisdavison"
export CHURNBOOK="$HOME/src/github.com/etsteam/churning/notebooks/develop/chris_davison"
export TEMPLATES="$CODE/ProjectTemplates"
export CHURNING_DATA_DIR="$HOME/.data/"
export LOGBOOK_DIR="$HOME/src/github.com/chrisdavison/logbook/"
export TASKDIR="$LOGBOOK_DIR/tasks/"
export RX_mdlink="\[.*\]\(.*\)|\[.*\]: .*"

# Variables representing GTD/ZTD files
export TODOFILE="$HOME/Dropbox/n/notes/todo.txt"
export DONEFILE="$HOME/Dropbox/n/notes/done.txt"
export EXTERNAL_BRAIN="$HOME/Dropbox/n/notes/capture.txt"
