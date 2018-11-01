# ========= ZSH
export ZSH="/Users/davison/.oh-my-zsh"

ZSH_THEME="flazz"
DISABLE_AUTO_UPDATE="true"
plugins=(
  
)

source $ZSH/oh-my-zsh.sh

# ====== Personal config
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

path+=$HOME/.vim/bundle/fzf/bin
path+=/usr/local/lib/node_modules
path+=$GOBIN
path+=$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin
path+=/Users/davison/Library/Python/3.7/bin/
path+=/Applications/Julia-1.0.app/Contents/Resources/julia/bin/
path+=$HOME/devel/scripts/
path+=$HOME/.cargo/bin/
path+=$HOME/bin
path+=/usr/local/miniconda3/bin

# sourceOrError "$DOTFILES/prompt.zsh"
source "$DOTFILES/functions.zsh"
source "$DOTFILES/aliases.zsh"
source "$DOTFILES/bindings.zsh"
source "$DOTFILES/fzf.sh"

typeset -aU path

[ -f "$HOME/.cargo/env" ] && source $HOME/.cargo/env
[ -f "/usr/local/etc/profile.d/autojump.sh" ] && source /usr/local/etc/profile.d/autojump.sh
[ -x pipenv ] && eval $(pipenv --completion)
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
