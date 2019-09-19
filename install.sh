echo "Downloading Junegunn/Plug for vim"
if [ "$OSTYPE" = "msys" ]; then
    vimdir="vimfiles"
else
    vimdir=".vim"
fi

curl -fLo ~/$vimdir/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo "Downloading Junegunn/fzf for fuzzy finding"
if [ ! -d "$HOME/.fzf" ]; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
else
    echo "FZF already cloned"
    cd ~/.fzf
    git pull --rebase > /dev/null
fi
~/.fzf/install --all > /dev/null

for f in .bin .config .emacs.d .vim .bashrc .gitconfig .sqliterc .tmux.conf .vimrc .zshrc ; do
    echo $f
    # Need to use gcp for gnu-coreutils on osx
    # do an OS-check to see if I can use cp on linux
    gcp -Tabs ~/code/dotfiles/$f ~/$f
done

