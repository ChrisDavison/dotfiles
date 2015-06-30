#!/usr/bin/env sh
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="pure"

for file in ~/.dotfiles/{aliases,exports,functions}; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(pep8 autopep8)

source $ZSH/oh-my-zsh.sh

PERL_MB_OPT="--install_base \"/Users/davison/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/davison/perl5"; export PERL_MM_OPT;
