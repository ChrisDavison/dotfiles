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
export RESEARCHFIGURES="$HOME/Dropbox/work/figures"

export GOPATH="$HOME"
export GOBIN="$GOPATH/bin"

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

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

sourceOrError() {
    [ -f $1 ] && source $1 || echo "No $1"
}

sourceOrError "$DOTFILES/functions.zsh"
sourceOrError "$DOTFILES/aliases.zsh"
sourceOrError "$DOTFILES/prompt.zsh"
sourceOrError "$DOTFILES/bindings.zsh"
sourceOrError ~/.fzf.zsh
sourceOrError ~/.cargo/env
sourceOrError /usr/local/etc/profile.d/autojump.sh
