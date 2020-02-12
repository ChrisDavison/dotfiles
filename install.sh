#!/bin/bash
CODEDIR=$HOME/src/github.com
DOTFILES=$CODEDIR/ChrisDavison/dotfiles

echo "============================"
echo "get submodules (vim plugins)"
echo "============================"
git submodule update --init

echo "======================"
echo "symlinking plain files"
echo "======================"
for f in .bashrc .gitconfig .sqliterc .tmux.conf .vimrc .zshrc ; do
    if [ -f $HOME/$f ] || [ -h $HOME/$f ]; then
        rm $HOME/$f
    fi
    ln -s $ln_verbose $DOTFILES/$f $HOME/$f
done

echo "============================="
echo "symlinking vim and emacs dirs"
echo "============================="
for direc in .vim .emacs.d ; do
    if [ -h $HOME/$direc ] || [ -d $HOME/$direc ]; then
        rm -rf $HOME/$direc
    fi
    ln -s $ln_verbose $DOTFILES/$direc $HOME/$direc
done

echo "==========================="
echo "symlinking stuff in .config"
echo "==========================="
for direc in .config/* ; do
    base=$(basename $direc)
    if [ -h $HOME/$direc ] || [ -d $HOME/$direc ]; then
        rm -rf $HOME/$direc
    fi
    ln -s $ln_verbose $DOTFILES/.config/$base $HOME/.config/$base
done

echo "==================="
echo "symlinking binaries"
echo "==================="
for bin in .bin/* ; do
    base=$(basename $bin)
    if [ -f $HOME/.bin/$base ] || [ -h $HOME/.bin/$base ] ; then
        rm $HOME/.bin/$base
    fi
    ln -s $ln_verbose $DOTFILES/.bin/$base $HOME/.bin/$base
    chmod +x $DOTFILES/.bin/$base
done

echo "===================="
echo "downloading my repos"
echo "===================="
for repo in checkmark animalhash repoutil scripts seqname tagsearch thesis thirtyday timer vim-cdroot;
do
    if [ ! -d "$CODEDIR/ChrisDavison/$repo" ]; then
        [ $verbose -eq 1 ] && echo "\t$repo"
        git clone --quiet git@github.com:ChrisDavison/"$repo" $CODEDIR/ChrisDavison/"$repo" > /dev/null
    fi
done

echo "======================"
echo "downloading work repos"
echo "======================"
for repo in cattleprod collar-outlier-removal cowhealth precisionbeef ee273 cybele-sat bolus heatstress iof;
do
    if [ ! -d "$CODEDIR/cidcom/$repo" ]; then
        [ $verbose -eq 1 ] && echo "\t$repo"
        git clone --quiet git@github.com:cidcom/"$repo" $CODEDIR/cidcom/"$repo" > /dev/null
    fi
done

echo "==========="
echo "install fzf"
echo "==========="
$DOTFILES/.vim/bundle/fzf/install --all > /dev/null
