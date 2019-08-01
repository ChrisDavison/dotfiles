export ZSH="/Users/davison/.oh-my-zsh"
ZSH_THEME="avit"
HIST_STAMPS="yyyy-mm-dd"
plugins=()

source $ZSH/oh-my-zsh.sh

source ~/.envs/ml/bin/activate

source_or_error() {
    [ -f "$1" ] && source "$1" || echo "No file: $1"
}
source_or_error ~/.fzf.zsh
source_or_error /usr/local/bin/virtualenvwrapper.sh

if [ $(basename $SHELL) = "zsh" ]; then
    source_or_error "$CODEDIR/dotfiles/setopts.zsh"
fi

source_or_error "$CODEDIR/dotfiles/paths_and_exports.bash"
source_or_error "$CODEDIR/dotfiles/aliases.bash"
source_or_error "$CODEDIR/dotfiles/prompt.zsh"
source_or_error $SHELLFUNCS
source_or_error $HOME/.cargo/env

# ~/bin/randomquote || echo "No bin/randomquote"
export PATH="/usr/local/sbin:$PATH"
