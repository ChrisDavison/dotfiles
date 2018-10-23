SAVEHIST=100000
HISTSIZE=100000
HISTFILE=~/.zsh_history

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent
setopt auto_param_slash
setopt complete_in_word
setopt glob_complete
setopt list_beep
setopt list_packed
setopt list_rows_first
setopt no_beep
setopt append_history
unsetopt share_history
unsetopt bang_hist
unsetopt extended_history

export DOTFILES="$HOME/devel/dotfiles"
export TERM=xterm-256color
export EDITOR="vim"
export LOGBOOK_DIR="$HOME/devel/logbook"
export RESEARCHFIGURES="$HOME/Dropbox/work/outputs"

export GOPATH="$HOME"
export GOBIN="$GOPATH/bin"
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export CAPTUREFILE=$HOME/Dropbox/.capture
export RESEARCHFIGURES=$HOME/Dropbox/work/figures

# Add various directories to path
_append_to_path() {
  if [ -d $1 -a -z ${path[(r)$1]} ]; then
    path=($path $1);
  fi
}
_append_to_path ~/.vim/bundle/fzf/bin
_append_to_path /usr/local/lib/node_modules
_append_to_path $GOBIN
_append_to_path $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin
_append_to_path /Users/davison/Library/Python/3.7/bin/
_append_to_path /Applications/Julia-1.0.app/Contents/Resources/julia/bin/
_append_to_path ~/devel/scripts/
_append_to_path ~/.cargo/bin/
_append_to_path $HOME/bin
_append_to_path /usr/local/miniconda3/bin
if [ -f /usr/local/share/autojump/autojump ]; then
    source /usr/local/share/autojump/autojump
fi


sourceOrError() {
    [ -f $1 ] && source $1 || echo "No $1"
}

sourceOrError "$DOTFILES/functions.zsh"
sourceOrError "$DOTFILES/aliases.zsh"
# sourceOrError "$DOTFILES/prompt.zsh"
sourceOrError "$DOTFILES/bindings.zsh"
sourceOrError ~/.fzf.zsh
sourceOrError ~/.cargo/env
sourceOrError /usr/local/etc/profile.d/autojump.sh
if [ -x pipenv ]; then
    eval $(pipenv --completion)
fi

typeset -aU path
# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/davison/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="flazz"
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  
)

source $ZSH/oh-my-zsh.sh

