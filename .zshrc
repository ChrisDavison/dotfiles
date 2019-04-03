SAVEHIST=100000
HISTSIZE=100000
HISTFILE=~/.zsh_history

source_or_error() {
    [ -f "$1" ] && source "$1" || echo "No file: $1"
}

export CODEDIR="$HOME/code"

if [ $(basename $SHELL) = "zsh" ]; then
    source_or_error "$CODEDIR/dotfiles/setopts.zsh"
fi
source_or_error "$CODEDIR/dotfiles/paths_and_exports.bash"
source_or_error "$CODEDIR/dotfiles/aliases.bash"
source_or_error "$CODEDIR/dotfiles/prompt.zsh"
source_or_error $SHELLFUNCS
source_or_error $HOME/.cargo/env
source_or_error ~/.fzf.zsh
source_or_error /usr/local/bin/virtualenvwrapper.sh

# ~/bin/randomquote || echo "No bin/randomquote"
