To install

        cd ~
        git clone http://github.com/ChrisDavison/vimrc.git ~/.vim
        ln -s ~/.vim/vimrc ~/.vimrc
        ln -s ~/.vim/gvimrc ~/.gvimrc
        cd ~/.vim
        git submodule update --init

To update a submodule

    cd into directory of submodule
    git pull origin master

To update all submodules

    git submodule foreach git pull origin master
