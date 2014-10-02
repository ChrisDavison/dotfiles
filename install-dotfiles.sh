#!/bin/bash

rm -rf ~/.dotfile.old

# Make backup directory for old dotfiles
mkdir ~/.dotfile.old

# Backup existing dotfiles
# Symbolic link new dotfiles
for file in .{aliases,bashrc,exports,functions,vimrc,zshrc}; do
    mv ~/"$file" ~/.dotfile.old/.;
    ln -s ~/.dotfiles/"$file" ~/.;
done;
unset file;

# Now source bash and zsh
# Install homebrew
# Install software
# Install Python and virtualenvs
# Install Go, Rust, C, Haskell, Xcode command line
