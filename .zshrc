export DOTFILES="$HOME/devel/dotfiles"
export TERM=xterm-256color
export EDITOR="mvim"
export LOGBOOK_DIR="$HOME/devel/logbook"
export RESEARCHFIGURES="$HOME/Dropbox/work/figures"

export GOPATH="$HOME"
export GOBIN="$GOPATH/bin"

# Add various directories to path
export PATH=~/.vim/bundle/fzf/bin:$PATH;
export PATH=/usr/local/lib/node_modules:$PATH;
export PATH=$GOBIN:$PATH;
export PATH=$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin:$PATH;
export PATH=/Users/davison/Library/Python/3.7/bin/:$PATH;
export PATH=/Applications/Julia-1.0.app/Contents/Resources/julia/bin/:$PATH;

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
