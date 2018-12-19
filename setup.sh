DIR="$1"
if [ "$DIR" = "" ]; then
    echo "Must pass dotfiles directory"
    exit
fi
# Prepare symlinks {{{
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
# }}}
# Clone vim plugins, using pathogen method {{{
curl --create-dirs -sSLo ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
if [ "$OSTYPE" = "msys" ]; then
    vimdir="vimfiles"
else
    vimdir=".vim"
fi
clone_to_bundle() {
    repo="$1"
        if [ ! -d ~/"$vimdir"/bundle ]; then
        mkdir -p ~/"$vimdir"/bundle
    fi
    shortname=$(echo "$repo" | sed -e "s_/_-_")
    echo "$shortname" >> ~/.vim-plugins
    target=~/"$vimdir"/bundle/$shortname
    if [ ! -d "$target" ]; then
        git clone git@github.com:"$repo" "$target" > /dev/null
    fi
}
# }}} 
# Install FZF {{{
if [ ! -d "$HOME/.vim/bundle/fzf" ]; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.vim/bundle/fzf
else
    echo "FZF already cloned"
    cd ~/.vim/bundle/fzf
    git pull --rebase > /dev/null
fi
~/.vim/bundle/fzf/install --all > /dev/null
# }}}
