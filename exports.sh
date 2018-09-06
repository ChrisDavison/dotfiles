# Exports for bash/zsh
[[ -x /usr/local/bin/nvim ]] && export EDITOR="nvim" || export EDITOR="vim"

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
export RESEARCHFIGURES="$HOME/Dropbox/work/figures/"
export JULIAPATH="/Applications/Julia-1.0.app/Contents/Resources/julia/bin/"

export PATH=$JULIAPATH:$GOBIN:$TEXBIN:$NODEPATH:$HOMEBIN:$MULTIRUSTBIN:$STACKBIN:$PATH;

# 256 color terminal
export TERM=xterm-256color;

export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3

# Export various directories useful for mu work
export CHURNING_DATA_DIR="$HOME/.data/"
export LOGBOOK_DIR="$HOME/src/github.com/chrisdavison/logbook/"

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export PATH="/usr/local/opt/node@8/bin:$PATH"

