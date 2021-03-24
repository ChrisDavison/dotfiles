HISTFILE=$HOME/.zsh_history
HISTSIZE=100000
SAVEHIST=$HISTSIZE

setopt append_history # multiple shells can append to history
setopt hist_ignore_dups # remove older duplicate entries from history
setopt hist_ignore_all_dups # remove older duplicate entries from history
setopt hist_reduce_blanks # remove superfluous blanks from history items
setopt inc_append_history # save history entries as soon as they are entered
setopt share_history # share history between different instances of the shell

setopt auto_cd # cd by typing directory name if it's not a command
setopt cdablevars # cd by typing a variable, if variable is a directory
setopt auto_pushd # cd pushes directories onto the stack
setopt pushd_ignore_dups # don't push multiple copies of same dir onto stack

setopt auto_list # automatically list choices on ambiguous completion
setopt auto_menu # automatically use menu completion
setopt always_to_end # move cursor to end if word had one match

setopt no_beep #turn off terminal bell
setopt extended_glob
setopt interactive_comments # Allow comments in interactive shells

set -o emacs

