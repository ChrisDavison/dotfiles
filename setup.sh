DIR="$1"
if [ "$DIR" = "" ]; then
    echo "Must pass dotfiles directory"
    exit
fi

# symlink dotfiles
linkOrError(){
    target="$HOME"/"$1"
    origin="${DIR}/$1"
    if [ -f "${target}" ]; then
        echo "Replacing ${target}"
        rm "${target}"
    fi
    ln -s "${origin}" "${target}"
}
linkOrError ".zshrc"
linkOrError ".sqliterc"
linkOrError ".tmux.conf"
linkOrError ".gitconfig"
linkOrError ".vimrc"

if [ "$OSTYPE" = "msys" ]; then
    vimdir="vimfiles"
else
    vimdir=".vim"
fi

# JuneGunn's vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# FZF
if [ ! -d "$HOME/.fzf" ]; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
else
    echo "FZF already cloned"
    cd ~/.fzf
    git pull --rebase > /dev/null
fi
~/.fzf/install --all > /dev/null
