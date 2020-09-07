# Sensible Bash - An attempt at saner Bash defaults
# Maintainer: mrzool <http://mrzool.cc>
# Repository: https://github.com/mrzool/bash-sensible
# Version: 0.2.2
set -o vi

# Unique Bash version check
if ((BASH_VERSINFO[0] < 4))
then
  echo "sensible.bash: Looks like you're running an older version of Bash."
  echo "sensible.bash: You need at least bash-4.0 or some options will not work correctly."
  echo "sensible.bash: Keep your software up-to-date!"
fi

## GENERAL OPTIONS ##

# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

# Update window size after every command
shopt -s checkwinsize

# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

# Enable history expansion with space
# E.g. typing !!<space> will replace the !! with your last command
bind Space:magic-space

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

# Turn on extended globbing
# ?(pattern-list)   Matches zero or one occurrence of the given patterns
# *(pattern-list)   Matches zero or more occurrences of the given patterns
# +(pattern-list)   Matches one or more occurrences of the given patterns
# @(pattern-list)   Matches one of the given patterns
# !(pattern-list)   Matches anything except one of the given patterns
shopt -s extglob 2> /dev/null

# Turn on dot globbing (implicitly match . at start of filename, or after slash)
shopt -s dotglob 2> /dev/null

# Turn on null globbing (glob with no matches returns empty arg list)
shopt -s nullglob 2> /dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

## SMARTER TAB-COMPLETION (Readline bindings) ##

# Perform file completion in a case insensitive fashion
bind "set completion-ignore-case on"

# Treat hyphens and underscores as equivalent
bind "set completion-map-case on"

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

# Immediately add a trailing slash when autocompleting symlinks to directories
bind "set mark-symlinked-directories on"

## SANE HISTORY DEFAULTS ##

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

# Enable incremental history search with up/down arrows (also Readline goodness)
# Learn more about this here: http://codeinthehole.com/writing/the-most-important-command-line-tip-incremental-history-searching-with-inputrc/
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

## BETTER DIRECTORY NAVIGATION ##

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current working directory, in home and in the ~/projects folder
CDPATH="."

# This allows you to bookmark your favorite places across the file system
# Define a variable containing a path and you will be able to cd into it regardless of the directory you're in
shopt -s cdable_vars

# Examples:
# export dotfiles="$HOME/dotfiles"
# export projects="$HOME/projects"
# export documents="$HOME/Documents"
# export dropbox="$HOME/Dropbox"


##########################
# PERSONAL CONFIGURATION #
##########################

#####################
# PATHS AND EXPORTS #
#####################
export TERM=xterm-256color
export EDITOR="nvim"
export GOPATH="$HOME/go"
export GOBIN="$HOME/bin"
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export FZF_ALT_C_COMMAND='fd -t d . $HOME'
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export CODEDIR="$HOME/src/github.com"
export NOTESDIR="$HOME/Dropbox/notes"
export BUDGET_CONFIG="$HOME/Dropbox/house/income.csv"
export BUDGET_COSTS="$HOME/Dropbox/house/costs.csv"

export PATH=$HOME/.vim/bundle/fzf/bin:$PATH;
export PATH=$HOME/.bin:$PATH;
export PATH=/usr/local/lib/node_modules:$PATH;
export PATH=$GOBIN:$PATH;
export PATH=$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin:$PATH;
export PATH=/Users/davison/Library/Python/3.7/bin/:$PATH;
export PATH=/Applications/Julia-1.1.app/Contents/Resources/julia/bin/:$PATH;
export PATH=$CODEDIR/scripts/:$PATH;
export PATH=$HOME/.cargo/bin/:$PATH;
export PATH=$HOME/bin:$PATH;
export PATH=$HOME/.virtualenvs/:$PATH;
export PATH=/usr/local/miniconda3/bin:$PATH;
export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

export VIRTUALENVWRAPPER_PYTHON=`which python3`


###########
# ALIASES #
###########
if [ -x "$(command -v exa)" ]; then
    alias ls="exa --group-directories-first"
    alias ll="ls --long"
    alias la="ll -a"
    alias lt="exa --tree -L 2"
    alias lg="ll --git-ignore"
    alias ltg="lt --git-ignore"
elif [ -x "$(command -v gls)" ]; then
    alias ll='gls -lFh --group-directories-first --color=auto'
    alias la='gls -AlFh --group-directories-first --color=auto'
    alias ls='gls -CF --group-directories-first --color=auto'
    alias l='gls -CF --group-directories-first --color=auto'
else
    alias ll='ls -GlFh'
    alias la='ls -GAlFh'
    alias ls='ls -GCF'
    alias l='ls -GCF'
fi

alias c="clear"
alias cp="cp -rv"    # Always recursively and verbosely copy
alias mv="mv -v"     # Always explain move actions
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
alias rf="repoutil fetch"
alias rs="repoutil stat"
alias g="git"
alias today="date +%F"
alias tmux="tmux -2"
alias sedit="sudo $EDITOR"
alias envml="source $HOME/.envs/ml/bin/activate"
alias t="todo.sh -a -f -d $HOME/.todo/config"
alias habits="todo.sh -a -f -d $HOME/.todo/config ls +habit"
alias asmr="mpv \"$(randomasmr)\""

if [ ! $(uname -s) = 'Darwin' ];then
    if grep -q Microsoft /proc/version; then
        alias open='explorer.exe';
    else
        alias open='xdg-open';
    fi
fi

function o() {
    if [ $# -eq 0 ]; then
		open .;
	else
		open "$@";
	fi;
}

#############################
# SOURCE INSTALLED SOFTWARE #
#############################
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
[ -f $HOME/.cargo/env ] && source $HOME/.cargo/env
# source `which virtualenvwrapper.sh`

####################
# MY CUSTOM PROMPT #
####################
export PS1="\[\e[31m\][\[\e[m\]\[\e[35m\]\u\[\e[m\]@\[\e[32m\]\h\[\e[m\]:\W\[\e[31m\]]\[\e[m\] "

source ~/code/dotfiles/.functions

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# If not running interactively, don't do anything
case $- in
  *i*) ;;
    *) return;;
esac

# Path to the bash it configuration
export BASH_IT="/home/davison/.bash_it"

# .bash_it/themes/<theme>
export BASH_IT_THEME='norbu'

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='git@git.domain.com'

# Don't check mail when opening terminal.
unset MAILCHECK

# Change this to your console based IRC client of choice.
export IRC_CLIENT='irssi'

# Set this to the command you use for todo.txt-cli
export TODO="t"

# Set this to false to turn off version control status checking within the prompt for all themes
export SCM_CHECK=true

# Set Xterm/screen/Tmux title with only a short hostname.
# Uncomment this (or set SHORT_HOSTNAME to something else),
# Will otherwise fall back on $HOSTNAME.
#export SHORT_HOSTNAME=$(hostname -s)

# Set Xterm/screen/Tmux title with only a short username.
# Uncomment this (or set SHORT_USER to something else),
# Will otherwise fall back on $USER.
#export SHORT_USER=${USER:0:8}

# Set Xterm/screen/Tmux title with shortened command and directory.
# Uncomment this to set.
#export SHORT_TERM_LINE=true

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/djl/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# (Advanced): Uncomment this to make Bash-it reload itself automatically
# after enabling or disabling aliases, plugins, and completions.
# export BASH_IT_AUTOMATIC_RELOAD_AFTER_CONFIG_CHANGE=1

# Uncomment this to make Bash-it create alias reload.
# export BASH_IT_RELOAD_LEGACY=1

# Load Bash It
# source "$BASH_IT"/bash_it.sh
