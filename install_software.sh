echo "Downloading Junegunn/Plug for vim"
if [ "$OSTYPE" = "msys" ]; then
    vimdir="vimfiles"
else
    vimdir=".vim"
fi

curl -fLo ~/$vimdir/autoload/plug.vim -s --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo "Downloading Junegunn/fzf for fuzzy finding"
if [ ! -d "$HOME/.fzf" ]; then
git clone --quiet --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
else
    echo "Updating existing FZF"
    cd ~/.fzf
    git pull --rebase > /dev/null
fi
~/.fzf/install --all > /dev/null
