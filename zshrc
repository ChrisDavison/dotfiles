# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="cloud2"

for file in ~/.dotfiles/{aliases,exports,functions}; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

for file in ~/.{aliases,exports,functions}; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# Which plugins would you like to load?
# (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(pep8 autopep8)

source $ZSH/oh-my-zsh.sh

PERL_MB_OPT="--install_base \"/Users/davison/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/davison/perl5"; export PERL_MM_OPT;

# Use the 'thefuck' command
alias fuck='$(thefuck $(fc -ln -1))'
# You can use whatever you want as an alias, like for mondays:
alias FUCK='fuck'

alias g='git'
alias gst='git status'
alias grst='gr status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gc='git commit -v'
alias gc!='git commit -v --amend'
alias gco='git checkout'
alias gcom='git checkout master'
alias gb='git branch'
alias gba='git branch -a'
alias glg='git log --stat --max-count=10'
alias glog='git log --oneline --decorate --color --graph'
alias gap='git add -p'
alias ga='git add'
