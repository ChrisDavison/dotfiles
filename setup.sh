if [ -d ~/.dotfiles ]; then
    echo "Found dotfiles in ~/"
    DIR=~/.dotfiles
elif [ -d ~/src/github.com/chrisdavison/dotfiles ]; then
    echo "Found dotfiles in src/github.com/chrisdavison/"
    DIR=~/src/github.com/chrisdavison/dotfiles
else
    echo "Couldn't find dotfiles in usual locations..."
fi

echo "Downloading junegunn/plug for vim"
curl -sfLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

linkOrError(){
    target=~/"$1"
    origin="${DIR}/$1"
    if [ -f ${target} ]; then
        echo "${target} already exists. May need deletion & linking."
    else
        ln -s ${origin} ${target}
    fi
}
linkOrError ".zshenv"
linkOrError ".vimrc"
linkOrError ".sqliterc"
linkOrError ".tmux.conf"
linkOrError ".gitconfig"
vi +PlugInstall +PlugUpdate +PlugUpgrade
