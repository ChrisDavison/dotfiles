for fn in bashrc gitconfig sqliterc tmux.conf vimrc vim; do
    full_fn=$(pwd)/$fn
    target="$HOME/.$fn"
    if [ -f "$target" ] || [ -d "$target" ]; then
        read -r -p "'.$fn' exists in home.    Replace? [y/N] " response
        case "$response" in
            [yY][eE][sS]|[yY]) rm "$target" ;;
            *) ;;
        esac
    fi
    ln -s "$full_fn" "$target"
done

if [ "$OSTYPE" = "msys" ]; then
    vimdir="vimfiles"
else
    vimdir=".vim"
fi

# JuneGunn's vim-plug
curl -fLo ~/$vimdir/autoload/plug.vim --create-dirs \
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
