get_vim_plug() { # JuneGunn's vim-plug
    if [ "$OSTYPE" = "msys" ]; then
        vimdir="vimfiles"
    else
        vimdir=".vim"
    fi

    curl -fLo ~/$vimdir/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

get_junegunn_fzf() { # FZF
    if [ ! -d "$HOME/.fzf" ]; then
        git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    else
        echo "FZF already cloned"
        cd ~/.fzf
        git pull --rebase > /dev/null
    fi
    ~/.fzf/install --all > /dev/null
}

symlink_each_dotfile() { # Symlink dotfiles to ~
    dotfiles=(
        bashrc
        gitconfig
        sqliterc
        tmux.conf
        vimrc
        vim
    )

    for fn in ${dotfiles[@]}; do
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
}

symlink_each_dotfile
get_vim_plug
get_junegunn_fzf
