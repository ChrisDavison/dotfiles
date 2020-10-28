#!/.bin/bash
CODEDIR=$HOME/code

function remove_and_symlink() {
    [ -f $HOME/$1 ] && rm $HOME/$1
    ln -s $CODEDIR/dotfiles/$1 $HOME/$1
}

function remove_and_symlink_dir() {
    [ -d $HOME/$1 ] && rm $HOME/$1
    ln -s $CODEDIR/dotfiles/$1 $HOME/$1
}


echo "----- get submodules (vim plugins)"
git submodule update --init

echo "----- symlinking plain files"
remove_and_symlink .bashrc
remove_and_symlink .gitconfig
remove_and_symlink .sqliterc
remove_and_symlink .tmux.conf
remove_and_symlink .vimrc
remove_and_symlink .zshrc


echo "----- symlinking vim and emacs dirs"
remove_and_symlink_dir .vim
remove_and_symlink_dir .todo
remove_and_symlink_dir .todo.actions.d
# remove_and_symlink .emacs.d


echo "----- symlinking .config/ directories"
for direc in .config/* ; do
    base=$(basename $direc)
    if [ -h $HOME/$direc ] || [ -d $HOME/$direc ]; then
        rm -rf $HOME/$direc
    fi
    ln -s $ln_verbose $CODEDIR/dotfiles/.config/$base $HOME/.config/$base
done

echo "----- symlinking bin/ binaries"
if [ ! -d "$HOME/.bin" ]; then
    mkdir "$HOME/.bin"
fi
for bin in bin/* ; do
    base=$(basename $bin)
    if [ -f $HOME/.bin/$base ] || [ -h $HOME/.bin/$base ] ; then
        rm $HOME/.bin/$base
    fi
    ln -s $ln_verbose $CODEDIR/dotfiles/bin/$base $HOME/.bin/$base
    chmod +x $CODEDIR/dotfiles/bin/$base
done

echo "----- clone git repos from <dotfiles>/repos"
if [ -f "repos" ]; then
    while IFS= read -r repo; do
        if [ ! -d $CODEDIR/$(basename $repo) ]; then
            git clone --quiet git@github.com:$repo $CODEDIR/$(basename $repo) > /dev/null
            if [ $? -gt 0 ]; then
                echo "Error with: $repo"
            fi
        fi
    done < repos
fi

echo "----- install fzf (from within git dir)"
$CODEDIR/dotfiles/.vim/bundle/fzf/install --all > /dev/null
