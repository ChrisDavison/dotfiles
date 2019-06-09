[ -z "$PS1" ] && exit

shopt -s cdspell
shopt -s globstar
shopt -s extglob
shopt -s dotglob
shopt -s nullglob

export PS1="\W :: "

source_or_error(){
    if [ -f "$1" ]; then
        source "$1"
    else
        echo "No $1"
    fi
}

export DOTFILES=~/code/dotfiles
source_or_error $DOTFILES/paths_and_exports.bash
source_or_error $DOTFILES/functions.zsh
source_or_error $DOTFILES/aliases.bash
source_or_error ~/.fzf.bash

if grep -q "MINGW" <<< "$(uname -a)"; then
    export BROWSER=start
fi
