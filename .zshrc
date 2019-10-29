# ------------------- key bindings --------------------------------

# Ctrl-x Ctrl-e is a default binding for editing the current command line with
# $EDITOR in bash. Reproduce it for zsh.
autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# fix the insert/delete/home/end keys
typeset -A key
key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
[[ -n "${key[Home]}"   ]]  && bindkey "${key[Home]}"   beginning-of-line
[[ -n "${key[End]}"    ]]  && bindkey "${key[End]}"    end-of-line
[[ -n "${key[Insert]}" ]]  && bindkey "${key[Insert]}" overwrite-mode
[[ -n "${key[Delete]}" ]]  && bindkey "${key[Delete]}" delete-char

# ------------------- options ------------------------

setopt notify    # immediate job notifications
setopt autopushd # cd works like pushd


# ------------------- shared history --------------------------------

HISTSIZE=10000
SAVEHIST=10000
HISTFILE=${HISTFILE:-$HOME/.zsh_history}
setopt share_history
setopt hist_ignore_dups
setopt hist_ignore_space



# ------------------- enable menu completion ------------------------

autoload -U compinit && compinit
zstyle ':completion:*' menu select
# fix Shift-Tab in the completions menu
bindkey '^[[Z' reverse-menu-complete


# ------------------ disable terminal flow control ------------------

# Many terminals use Ctrl-s and Ctrl-q for flow control by default. This
# interferes with using Ctrl-r and Ctrl-s for history searching. Disable it.
stty stop undef


# ------------------- safe paste mode -------------------------------

# When pasting multiple lines into the terminal, create a single multi-line
# command that you can edit, instead of executing everything all at once.
# The following version is copied from oh-my-zsh.

# Code from Mikael Magnusson: http://www.zsh.org/mla/users/2011/msg00367.html
#
# Requires xterm, urxvt, iTerm2 or any other terminal that supports bracketed
# paste mode as documented: http://www.xfree86.org/current/ctlseqs.html

# create a new keymap to use while pasting
bindkey -N paste
# make everything in this keymap call our custom widget
bindkey -R -M paste "^@"-"\M-^?" paste-insert
# these are the codes sent around the pasted text in bracketed
# paste mode.
# do the first one with both -M viins and -M vicmd in vi mode
set -o emacs
bindkey '^[[200~' _start_paste
bindkey -M paste '^[[201~' _end_paste
# insert newlines rather than carriage returns when pasting newlines
bindkey -M paste -s '^M' '^J'

zle -N _start_paste
zle -N _end_paste
zle -N zle-line-init _zle_line_init
zle -N zle-line-finish _zle_line_finish
zle -N paste-insert _paste_insert

# switch the active keymap to paste mode
function _start_paste() {
  bindkey -A paste main
}

# go back to our normal keymap, and insert all the pasted text in the
# command line. this has the nice effect of making the whole paste be
# a single undo/redo event.
function _end_paste() {
#use bindkey -v here with vi mode probably. maybe you want to track
#if you were in ins or cmd mode and restore the right one.
  bindkey -e
  LBUFFER+=$_paste_content
  unset _paste_content
}

function _paste_insert() {
  _paste_content+=$KEYS
}

function _zle_line_init() {
  # Tell terminal to send escape codes around pastes.
  [[ $TERM == rxvt-unicode || $TERM == xterm || $TERM = xterm-256color || $TERM = screen || $TERM = screen-256color ]] && printf '\e[?2004h'
}

function _zle_line_finish() {
  # Tell it to stop when we leave zle, so pasting in other programs
  # doesn't get the ^[[200~ codes around the pasted text.
  [[ $TERM == rxvt-unicode || $TERM == xterm || $TERM = xterm-256color || $TERM = screen || $TERM = screen-256color ]] && printf '\e[?2004l'
}


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
alias cp="cp -rv"    # Always recursively and verbosely copy
alias mv="mv -v"     # Always explain move actions
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
alias rf="repoutil fetch"
alias rs="repoutil stat"

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
alias tmux="tmux -2"
alias sedit="sudo $EDITOR"

#############################
# SOURCE INSTALLED SOFTWARE #
#############################
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
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

# export PS1="\w \`parse_git_branch\`: "
# export PS1="\[\e[31m\][\[\e[m\]\[\e[35m\]\u\[\e[m\]@\[\e[32m\]\h\[\e[m\]:\W\[\e[31m\]]\[\e[m\] "
export PROMPT="[%1F%n%f@%2F%m %4F%1d%f] "
source ~/.functions

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
