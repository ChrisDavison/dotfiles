# Sensible Bash - An attempt at saner Bash defaults
# Maintainer: mrzool <http://mrzool.cc>
# Repository: https://github.com/mrzool/bash-sensible
# Version: 0.2.2

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
export CODEDIR="$HOME/code"
export NOTESDIR="$HOME/Dropbox/notes"

export PATH=$HOME/.vim/bundle/fzf/bin:$PATH;
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
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
alias rf="repofetch"
alias rs="repostat"

###############
# GIT ALIASES #
###############
alias g="git"
alias ga="git add"
alias gaa="git add --all"
alias gap="git add --patch"
alias gc="git commit -v"
alias gc!="git commit -v --amend"
alias gca="git commit -v -a"
alias gcl="git config --list"
alias gco="git checkout"
alias gss="git stash show --text"
alias gsc="git stash clear"
alias gwch="git whatchanged -p --abbrev-commit --pretty=medium"
alias gp="git pull"
alias gr="git record"
alias today="date +%F"

#############################
# SOURCE INSTALLED SOFTWARE #
#############################
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
[ -f /usr/local/bin/virtualenvwrapper.sh ] && source /usr/local/bin/virtualenvwrapper.sh
[ -f $HOME/.cargo/env ] && source $HOME/.cargo/env

####################
# MY CUSTOM PROMPT #
####################
# get current branch in git repo
function parse_git_branch() {
	BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
	if [ ! "${BRANCH}" == "" ]
	then
		STAT=`parse_git_dirty`
		echo "[${BRANCH}${STAT}]"
	else
		echo ""
	fi
}

# get current status of git repo
function parse_git_dirty {
	status=`git status 2>&1 | tee`
	dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
	untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
	ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
	newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
	renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
	deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
	bits=''
	if [ "${renamed}" == "0" ]; then
		bits=">${bits}"
	fi
	if [ "${ahead}" == "0" ]; then
		bits="*${bits}"
	fi
	if [ "${newfile}" == "0" ]; then
		bits="+${bits}"
	fi
	if [ "${untracked}" == "0" ]; then
		bits="?${bits}"
	fi
	if [ "${deleted}" == "0" ]; then
		bits="x${bits}"
	fi
	if [ "${dirty}" == "0" ]; then
		bits="!${bits}"
	fi
	if [ ! "${bits}" == "" ]; then
		echo " ${bits}"
	else
		echo ""
	fi
}

export PS1="\w \`parse_git_branch\`: "
#######################
# MY CUSTOM FUNCTIONS #
#######################
choose_tmux_session() {
    if tmux list-sessions 2>&1 > /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
}
alias tma=choose_tmux_session

inpath() { # Check ifa file is in $PATH
    type "$1" >/dev/null 2>&1;
}

swap() { # Swap two files (move, using a temporary)
    set -e
    mv "$2" "$1.$$"
    mv "$1" "$2"
    mv "$1.$$" "$1"
}

OpenInBrowser() { # Open link in whichever browser is in path
    read url
    [ -z $url ] && url="$@"
    [ -z $url ] && echo "Empty url" && return 1
    if inpath open; then
        open ${url}
    elif inpath firefox; then
        firefox ${url}
    elif inpath chrome; then
        chrome ${url}
    else
        echo "No browser..."
        return 2
    fi
}

noext() { # Remove extension from file
    echo "${1%.*}"
}

ppath() { # Pretty print $PATH
    echo "$PATH" | tr ':' '\n'
}

youtube() { # Get audio, video, or tidyurl from youtube
    [ $# -lt 1 ] && echo "Usage: youtube (video|audio|tidyurl) url" && return 1
    cmd=${1:-''}; shift
    case "$cmd" in
        video)
            tidied=$(youtube tidyurl "$1")
            format="%(title)s-%(id)s-%(format_id)s.%(ext)s"
            youtube-dl -f bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best --merge-output-format mp4 -o "$format" "$tidied"
            ;;
        audio)
            tidied=$(youtube tidyurl "$1")
            format="%(title)s-%(id)s-%(format_id)s.%(ext)s"
            youtube-dl --prefer-ffmpeg -f 171/251/140/bestaudio --extract-audio --audio-format mp3 --audio-quality 0 -o "$format" "$tidied"
            ;;
        tidyurl)
            echo "$1" | rg "&t=\d+s" -r '' | rg "&list=[a-zA-Z0-9_]+" -r '' | rg "&index=\d+" -r ''
            ;;
        *) echo "Usage: youtube (video|audio|tidyurl) url" ;;
    esac
}

mdtohtml() { # Convert a markdown file to html
    pandoc "$1" -o $(noext "$1").html --from markdown-simple_tables+subscript+superscript --filter pandoc-tablenos -s --toc --toc-depth=2 -c ~/src/github.com/chrisdavison/dotfiles/simple.css -s --mathjax
}

linkedtobin(){ # View all entires in ~/bin that are symlinks to my scripts
    ls -l ~/bin | awk -F' ' '/-> .*scripts.*/{print $7":"$9}' | column -s':' -t
}

add2md(){ # Add an asset to an md file as a link
    dest=$1
    dest_base=$(dirname $dest)
    file_dir="assets"
    target=$dest_base/$file_dir
    [ ! -f "$dest" ] && echo "No note file: $dest" && return 1
    [ ! -d "$target" ] && echo "No dir: $target" && return 2
    shift
    echo "Linking notes to $dest"
    for fn in $@
    do
        fn_short=$(basename $fn)
        echo "Move $fn_short to $target"
        echo "- [$fn_short](./$file_dir/$fn_short)" >> $dest
    done
    echo "===== TAIL OF THE NOTE FILE ====="
    tail -n $(( $# + 2 )) $dest
}

logbook() { # Open todays logbook in $EDITOR
    $EDITOR $(date +%"$HOME/Dropbox/notes/logbook/%Y/%Y-%m-%d.md")
}

logbook_recent() { # Display the last 10 logbooks
    bat $(fd . ~/Dropbox/notes/logbook | sort -r | head -n10) --style=header,grid
}
alias lbr="logbook_recent"

logbook_search() { # Display logbooks with contents matching query
    bat $(rg "$@" ~/Dropbox/notes/logbook -l | sort -r) --style=header,grid
}
alias lbs="logbook_search"

shfuncs() { # List shell functions, functions in bashrc
    rg "^[a-zA-Z_]+\(" ~/.bashrc | column -t -s '{'
}

nonascii() { # Ripgrep for non-ascii, greek, or "£"
    rg "[^\x00-\x7F£\p{Greek}]" -o --no-heading
}
