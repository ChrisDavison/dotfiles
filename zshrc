# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="pure"

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
alias gp='git push'


catless() {
    height=$(tput lines)
    fileheight=$(wc -l $1 | awk '{print $1}')
    if [[ $height -gt $fileheight ]]
    then
        cat $1
    else
        less $1
    fi
}

alias cl='catless'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Easier updating of Rust to nightly, until 1.0 is released
url="https://static.rust-lang.org/rustup.sh"
alias rustnightly='curl -s $url | sh'

# Make find easier to use
myfind() {
    find ./ -name "*$2*" -type "$1" | tee ${TMPDIR}/my_ffound | cat -n
    FFOUND_PWD=${PWD}
    FFOUND=($(cat ${TMPDIR}/my_ffound))
}
# And even simpler, for files and directories
ff() { myfind "f" "$1"; }
fd() { myfind "d" "$1"; }

# Extract filename from find list
fn() {
    [ ! -z ${FFOUND[$1-1]} ] && echo ${FFOUND_PWD}/${FFOUND[$1-1]};
}

# Open file from find list in vim, or change to the directory
v() { vim $(fn "$1");  }
ch() { cd $(dirname $(fn "$1")); }

# Re-list previous finds
lf() { echo ${FFOUND[*]} | tr -s ' ' '\n' | cat -n; }

# Easily jump back to Vim with CtrlZ
fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z
