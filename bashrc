# If not running interactively, don't do anything
[ -z "$PS1" ] && return

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
CDPATH=".:~:~/src/github.com/chrisdavison"

# This allows you to bookmark your favorite places across the file system
# Define a variable containing a path and you will be able to cd into it regardless of the directory you're in
shopt -s cdable_vars

# Terminal history handling
export HISTFILE=~/.history
export SAVEHIST=9000

# ==============================
#        DIRECTORIES etc
# ==============================
[[ -x /usr/local/bin/nvim ]] && export EDITOR="nvim" || export EDITOR="vim"
# Environment variables/path
export GOPATH="$HOME";
export GOBIN="$HOME/bin";
export HOMEBIN="$GOBIN"
export MULTIRUSTBIN="$HOME/.multirust/toolchains/nightly/cargo/bin"
export RESEARCHFIGURES="$HOME/Dropbox/f/figures/"
export PATH=$GOBIN:$NODEPATH:$HOMEBIN:$MULTIRUSTBIN:$PATH;

# Export various directories useful for mu work
export CODE="$HOME/src/github.com/chrisdavison"
export CHURNBOOK="$HOME/src/github.com/etsteam/churning/notebooks/develop/chris_davison"
export TEMPLATES="$CODE/ProjectTemplates"
export CHURNING_DATA_DIR="$HOME/.data/"
export LOGBOOK_DIR="$HOME/src/github.com/chrisdavison/logbook/"
export TASKDIR="$LOGBOOK_DIR/tasks/"
export TASKSDIR=("$LOGBOOK_DIR/tasks/" "$HOME/Dropbox/t/tasks")

# 256 color terminal
export TERM=xterm-256color;

# Variables representing GTD/ZTD files
export TODOFILE="$HOME/Dropbox/n/notes/todo.txt"
export DONEFILE="$HOME/Dropbox/n/notes/done.txt"
export EXTERNAL_BRAIN="$HOME/Dropbox/n/notes/capture.txt"

# ===================================
#       SOURCE INSTALLED SCRIPTS
# ===================================
sourceOrErrorMessage(){
    [ -f "$1" ] && source "$1" || echo "$1 doesn't exist"
}
sourceOrErrorMessage /usr/local/etc/profile.d/autojump.sh


MDTidy() {
    if [ "$1" = 1 ]; then
        wrap='--columns=80'
    else
        wrap='--wrap=none'
    fi
    shift
    if [ "$1" = 1 ]; then
        ref='--reference-links'
    else
        ref=''
    fi
    shift
    to='markdown_github-hard_line_breaks+line_blocks+tex_math_dollars-shortcut_reference_links'
    extra='-s --atx-headers'
    pandoc -t ${to} ${extra} ${ref} ${wrap} $@
}

alias g="git"
alias mdtidy='MDTidy 0 0'
alias mdtidyref='MDTidy 0 1'
alias mdtidywrap='MDTidy 1 0'
alias mdtidywrapref='MDTidy 1 1'

# Directory listing aliases
# ver=$(ls --version | egrep -o "GNU")
# if [ -n "$ver" ]; then
#     echo "Using GNU ls"
#     alias ll='gls -lFh --group-directories-first --color=auto'
#     alias la='gls -AlFh --group-directories-first --color=auto'
#     alias l='gls -CF --group-directories-first --color=auto'
# else
    echo "Using standard OSX ls"
    alias ll='ls -GlFh'
    alias la='ls -GAlFh'
    alias l='ls -GCF'
# fi

# ==============================
#         FROM ZSHRC
# ==============================
alias c='clear'
alias t="todo.sh -@"
alias tw="todo.sh -d ~/.todo/config-work -@"
alias d="davison"
alias gitsync="gr git fetch --all"
alias gitdown="gr git pull --rebase"
alias gst="gr status"
alias dr="gr status | rg 'ahead|behind|modified'"
alias dkmnt='docker run -it --rm -v /c/Users/user01/home:/ehome'
alias dpandoc='docker run -it --rm -v /c/Users/user01/notes:/source jagregory/pandoc'
alias tma='tmux attach -d -t $(tmux list-sessions | fzf | cut -d: -f1)'
alias tmuxhere='tmux new -s $(basename $(pwd))'

# Make ripgrep Smart-case search by default
alias rg='rg -S'

export LOGBOOK_DIR="$HOME/src/github.com/chrisdavison/logbook/"
export TASKDIR="$LOGBOOK_DIR/tasks/"

# Variables representing GTD/ZTD files
export TODOFILE="$HOME/Dropbox/n/notes/todo.txt"
export DONEFILE="$HOME/Dropbox/n/notes/done.txt"
export CAPTUREDIR="$HOME/Dropbox/n/notes/_dump/"
export TASKPATHS=("$HOME/Dropbox/t/tasks" "$HOME/src/github.com/chrisdavison/logbook/tasks")

#----------------------------------------------------------------------------#
# Bash text colour specification:  \e[<STYLE>;<COLOUR>m
# (Note: \e = \033 (oct) = \x1b (hex) = 27 (dec) = "Escape")
# Styles:  0=normal, 1=bold, 2=dimmed, 4=underlined, 7=highlighted
# Colours: 31=red, 32=green, 33=yellow, 34=blue, 35=purple, 36=cyan, 37=white
#----------------------------------------------------------------------------#
export PROMPT_COMMAND=prompt_cmd
prompt_cmd(){
    history -a
    history -c
    history -r
    local ex=$?
    local col="\e[0;32m"
    if [ "$ex" -gt 0 ]; then
        col="\e[0;31m" 
    fi
    PS1="\e[1;37m\w $col>>\e[0;37m " 
}

# ==============================
#  Find resource-heavy commands
# ==============================
alias memHogsTop='top -l 1 -o rsize | head -20'
alias memHogsPs='ps wwaxm -o pid,stat,vsize,rss,time,command | head -10'
alias cpu_hogs='ps wwaxr -o pid,stat,%cpu,time,command | head -10'
alias topForever='top -l 9999999 -s 10 -o cpu'
alias ttop="top -R -F -s 10 -o rsize"

# Networking
alias myip='curl ip.appspot.com'

choose_vim() {
    $EDITOR -S $(find ~/Dropbox/v/vim -name "*.vim" -type f | fzf)
}
alias cv='choose_vim'

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
source ~/.cargo/env

PATH="/Users/davison/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/davison/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/davison/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/davison/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/davison/perl5"; export PERL_MM_OPT;

is_in_git_repo() {
  git rev-parse HEAD > /dev/null 2>&1
}

fzf-down() {
  fzf --height 50% "$@" --border
}

gf() {  # Use FZF to step through git diffs
  is_in_git_repo || return
  git -c color.status=always status --short |
  fzf-down -m --ansi --nth 2..,.. \
    --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' |
  cut -c4- | sed 's/.* -> //'
}

gb() {  # Use FZF to step through git branches
  is_in_git_repo || return
  git branch -a --color=always | grep -v '/HEAD\s' | sort |
  fzf-down --ansi --multi --tac --preview-window right:70% \
    --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1) | head -'$LINES |
  sed 's/^..//' | cut -d' ' -f1 |
  sed 's#^remotes/##'
}

gt() {  # Use FZF to step through git tags
  is_in_git_repo || return
  git tag --sort -version:refname |
  fzf-down --multi --preview-window right:70% \
    --preview 'git show --color=always {} | head -'$LINES
}

gh() {  # Use FZF to step through git commit hashes
  is_in_git_repo || return
  git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
  fzf --height 80% "$@" --border --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
    --header 'Press CTRL-S to toggle sort' \
    --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always | head -'$LINES |
  grep -o "[a-f0-9]\{7,\}"
}

cap(){
    msg="$1";
    line1=$(echo "$msg" | head -n1 | tr " " "-" | tr "[:upper:]" "[:lower:]")
    filename="$CAPTUREDIR$(date +%F)--$line1.md"
    echo "$msg" | awk "NR>1{print}" > "$filename"
}

# Bash completions, from homebrew
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
export CAPTUREDIR="$HOME/Dropbox/n/notes/_dump/"