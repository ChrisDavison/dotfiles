#!/bin/bash
CODEDIR=$HOME/src/github.com
DOTFILES=$CODEDIR/ChrisDavison/dotfiles

cmd=$1
verbose=0
for arg in $@ ; do
    if [ $arg = "-v" ]; then
        verbose=1
        ln_verbose="-v"
        break
    fi
done


_headertext(){
    msg=$1
    if [ $verbose -eq 1 ] ; then
        echo "===== $msg =====" | tr "[:lower:]" "[:upper:]"
    fi
}

install_symlinks() {
    install_symlinks_to_plain_files
    install_symlinks_to_dirs
    install_symlinks_to_bins
}

install_symlinks_to_plain_files(){
    _headertext "symlinking plain files"
    for f in .bashrc .gitconfig .sqliterc .tmux.conf .vimrc .zshrc ; do
        if [ -f $HOME/$f ] || [ -h $HOME/$f ]; then
            rm $HOME/$f
        fi
        ln -s $ln_verbose $DOTFILES/$f $HOME/$f
    done
}

install_symlinks_to_dirs(){
    _headertext "symlinking vim and emacs dirs"
    for direc in .vim .emacs.d ; do
        if [ -h $HOME/$direc ] || [ -d $HOME/$direc ]; then
            rm -rf $HOME/$direc
        fi
        ln -s $ln_verbose $DOTFILES/$direc $HOME/$direc
    done

    _headertext "symlinking stuff in .config"
    for direc in .config/* ; do
        base=$(basename $direc)
        if [ -h $HOME/$direc ] || [ -d $HOME/$direc ]; then
            rm -rf $HOME/$direc
        fi
        ln -s $ln_verbose $DOTFILES/.config/$base $HOME/.config/$base
    done
}

install_symlinks_to_bins(){
    _headertext "symlinking binaries"
    for bin in .bin/* ; do
        base=$(basename $bin)
        if [ -f $HOME/.bin/$base ] || [ -h $HOME/.bin/$base ] ; then
            rm $HOME/.bin/$base
        fi
        ln -s $ln_verbose $DOTFILES/.bin/$base $HOME/.bin/$base
        chmod +x $DOTFILES/.bin/$base
    done
}

install_personal_repos () {
    _headertext "downloading my repos"
    for repo in ChrisDavison-hugo learning scripts;
    do
        if [ ! -d "$CODEDIR/ChrisDavison/$repo" ]; then
            [ $verbose -eq 1 ] && echo "\t$repo"
            git clone --quiet git@github.com:ChrisDavison/"$repo" $CODEDIR/ChrisDavison/"$repo" > /dev/null
        fi
    done
}

install_work_repos(){
    _headertext "downloading work repos"
    for repo in cattleprod collar-outlier-removal cowhealth precisionbeef ee273 cybele-sat ;
    do
        if [ ! -d "$CODEDIR/cidcom/$repo" ]; then
            [ $verbose -eq 1 ] && echo "\t$repo"
            git clone --quiet git@github.com:cidcom/"$repo" $CODEDIR/cidcom/"$repo" > /dev/null
        fi
    done
}

install_plug() {
    _headertext "Installing junegunn/vim-plug"
    if [ "$OSTYPE" = "msys" ]; then
        vimdir="vimfiles"
    else
        vimdir=".vim"
    fi

    curl -fLo $HOME/$vimdir/autoload/plug.vim -s --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

    mkdir -p $HOME/.config/nvim/autoload/
    cp $HOME/$vimdir/autoload/plug.vim $HOME/.config/nvim/autoload/plug.vim
}

install_fzf() {
    _headertext "Downloading Junegunn/fzf for fuzzy finding"
    if [ ! -d "$HOME/.fzf" ]; then
    git clone --quiet --depth 1 https://github.com/junegunn/fzf.git $HOME/.fzf
    else
        [ $verbose -eq 1 ] && echo "Updating existing FZF"
        cd $HOME/.fzf
        git pull --rebase > /dev/null
    fi
    $HOME/.fzf/install --all > /dev/null
}

usage() {
    echo 'usage: install.sh (all|fzf|plug|repos|software|symlinks)'
}


case $cmd in
    all)
        _headertext "Installing everything"
        install_symlinks
        install_fzf
        install_plug
        install_personal_repos
        install_work_repos
        ;;
    fzf) install_fzf ;;
    plug) install_plug ;;
    repos) install_personal_repos && install_work_repos ;;
    symlinks) install_symlinks ;;
    *) usage; exit 1 ;;
esac
