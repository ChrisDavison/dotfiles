#!/usr/bin/env bash
CODEDIR=$HOME/code

#=========================================================== 
echo "Symlinking files"
plain_files=(
    ".bashrc"
    ".gitconfig"
    ".sqliterc"
    ".tmux.conf"
    ".vimrc"
    ".zshrc"
)
for thing in "${plain_files[@]}"; do
    source="$CODEDIR/dotfiles/$thing"
    target="$HOME/$thing"
    if [ -f "$target" ]; then
        rm "$target"
    fi
    ln -s "$source" "$target"
done
unset plain_files; unset source; unset target;

#=========================================================== 
echo "Symlinking directories"
directories=(
    ".vim"
    # ".emacs.d"
    # ".todo"
    # ".todo.actions.d"
)
for thing in "${directories[@]}"; do
    source="$CODEDIR/dotfiles/$thing"
    target="$HOME/$thing/"
    if [ -d "$target" ]; then
        rm "$target"
    fi
    ln -s "$source" "$target"
done
unset directories; unset source; unset target;

#=========================================================== 
echo "Symlinking .config/ directories"
for direc in .config/* ; do
    base=$(basename $direc)
    if [ -h $HOME/$direc ] || [ -d $HOME/$direc ]; then
        rm -rf $HOME/$direc
    fi
    ln -s $ln_verbose $CODEDIR/dotfiles/.config/$base $HOME/.config/$base
done

#=========================================================== 
echo "Symlinking bin/ binaries"
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

#=========================================================== 
echo "----- clone git repos from <dotfiles>/repos"
if [ -f "$CODEDIR/dotfiles/repos" ]; then
    while IFS= read -r repo; do
        if [ ! -d $CODEDIR/$(basename $repo) ]; then
            git clone --quiet git@github.com:$repo $CODEDIR/$(basename $repo) > /dev/null
            if [ $? -gt 0 ]; then
                echo "Error with: $repo"
            fi
        fi
    done < "$CODEDIR/dotfiles/repos"
fi

#=========================================================== 
echo "Installing fzf"
if ! type fzf > /dev/null; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
fi

#=========================================================== 
echo "Installing rust"
if ! type cargo > /dev/null; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

#=========================================================== 
echo "Installing rust utilities"
echo "  fd"
cargo install -q fd-find
echo "  exa"
cargo install -q exa
echo "  bat"
cargo install -q bat
echo "  ripgrep"
cargo install -q ripgrep
echo "  starship"
cargo install -q starship

#=========================================================== 
echo "Installing doom emacs"
if ! type "$HOME/.emacs.d/bin/doom" > /dev/null; then
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom install
fi

# echo "----- get submodules (vim plugins)"
# git submodule update --init
