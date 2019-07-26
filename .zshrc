export ZSH="/Users/davison/.oh-my-zsh"
ZSH_THEME="imajes"
HIST_STAMPS="yyyy-mm-dd"
plugins=()

source $ZSH/oh-my-zsh.sh

source ~/.envs/ml/bin/activate

source_or_error() {
    [ -f "$1" ] && source "$1" || echo "No file: $1"
}
source_or_error ~/.fzf.zsh
source_or_error /usr/local/bin/virtualenvwrapper.sh
