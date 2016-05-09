#!/usr/bin/env sh
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="honukai"

# plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(pep8 autopep8)

# Source oh-my-zsh BEFORE my sourcing
# as it has it's own aliases that overlap mine
source $ZSH/oh-my-zsh.sh

for file in ~/.dotfiles/{aliases,exports,functions}; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# Hook for desk activation
[ -n "$DESK_ENV" ] && source "$DESK_ENV"


