########################################
#####           ENV VARS          ######
########################################
export EDITOR="nvim"
if [[ -x "$HOME/.bin/nvim.appimage" ]]; then
    export EDITOR="$HOME/.bin/nvim.appimage"
fi
export GOPATH="$HOME"
export GOBIN="$HOME/.bin"
if [[ -x $(which fd) ]] || [[ -x $(which fdfind) ]]; then
    export FZF_DEFAULT_COMMAND="fd -H -E '.git' -E '.keep' --type file --follow"
    export FZF_ALT_C_COMMAND='fd -t d . $HOME'
else
    export FZF_DEFAULT_COMMAND='rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
fi
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export VIRTUAL_ENV_DISABLE_PROMPT=1
export MAIL=$HOME/.mbox
export RE_UUID="[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}"
export RANGER_LOAD_DEFAULT_RC=0
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library"
export BROWSER="firefox"
export LC_ALL=en_GB.UTF-8

paths=($HOME/bin $HOME/.bin $HOME/.fzf/bin $HOME/code/scripts $HOME/.cargo/bin $HOME/.local/bin $HOME/.nimble/bin $HOME/.local/go/bin /usr/local/go/bin $HOME/code/dotfiles/bin $HOME/.rbenv/bin $HOME/.rbenv/versions/3.0.0/bin)
for p in ${paths[@]}; do
    export PATH="$p":$PATH
done
typeset -U path
typeset -U PATH

########################################
#####            SETTINGS          ##### 
########################################
HISTFILE=$HOME/.zsh_history
HISTSIZE=100000
SAVEHIST=$HISTSIZE

setopt hist_ignore_all_dups # remove older duplicate entries from history
setopt hist_reduce_blanks # remove superfluous blanks from history items
setopt inc_append_history # save history entries as soon as they are entered
setopt share_history # share history between different instances of the shell
setopt auto_cd # cd by typing directory name if it's not a command
setopt auto_pushd # cd pushes directories onto the stack
setopt auto_list # automatically list choices on ambiguous completion
setopt auto_menu # automatically use menu completion
setopt always_to_end # move cursor to end if word had one match
setopt no_beep #turn off terminal bell
setopt extended_glob

set -o emacs

# completion
zstyle ':completion:*' menu select # select completions with arrow keys
zstyle ':completion:*' group-name '' # group results by category
zstyle ':completion:::::' completer _expand _complete _ignored _approximate #enable approximate matches for completion

autoload -Uz compinit;compinit -i

# keybinds
# up and down do history search
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward

########################################
#####          ALIASES             #####
########################################

alias bm="bookmarks"
alias clip="xclip -sel clipboard"
alias cp="cp -rv"    # Always recursively and verbosely copy
alias df="df -x squashfs"
alias dls="cat ~/.download"
alias g="git"
alias inbox="nvim $HOME/code/knowledge/inbox.txt"
alias ipython="ipython --pprint --no-banner"
alias less='less -R'    # Use color codes in 'less'
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias mv="mv -v"     # Always explain move actions
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias rgl="rg --multiline --multiline-dotall"
alias sz="source ~/.zshrc"
alias tmux="TERM=xterm-256color tmux -2"
alias today="date +%F"
alias ts="tagsearch"
alias v="nvim"
alias zc="ziputil choose"
alias zv="ziputil view"

# alias t="todo.sh"
# alias tt="todo.sh due"
# alias tw="todo.sh due @work"

# alias n="echo '-  $argv' >> $HOME/code/knowledge/inbox.txt"
# alias nt="echo '-  [ ] $argv' >> $HOME/code/knowledge/inbox.txt"
# alias n="note.py"

[[ -x "$HOME/.bin/nvim.appimage" ]] && alias v="$HOME/.bin/nvim.appimage"

# aliases (conditional)
 
# fd was installed from apt, so is installed as fdfind to not shadow
# another 'fd' command
[[ -x $(which fdfind) ]] && alias fd="fdfind"

alias ru="repoutil unclean"
alias rs="repoutil stat"
alias rl="repoutil list"
alias rf="repoutil fetch"
alias rb="repoutil branchstat | sed -e 's/.*code\///' | sort | column -s'|' -t"

if [[ -x "$HOME/.cargo/bin/exa" ]]; then
    default_exa="exa --group-directories-first"
    alias ls="$default_exa"
    alias ll="$default_exa --long --git"
    alias la="$default_exa --long -a --git"
else
    alias ls="ls --color --group-directories-first"
    alias ll="ls -l"
    alias la="ls -l -a"
fi
alias dlaq="dla -q"

########################################
#####         FUNCTIONS            #####
########################################
inpath() { # Check ifa file is in $PATH
    type "$1" >/dev/null 2>&1;
}

cdrepo(){
    query=""
    if [[ $# -gt 0 ]]; then
        query="-q $@"
    fi
    chosen=`fd . -t d -d 1 $HOME/code | fzf --tac -1 -0 $query`
    if [[ ! -z $chosen ]]; then
        cd "$chosen"
    fi
}
alias cdr="cdrepo"

logbook() { # Open todays logbook in $EDITOR
    $EDITOR $(date +%"$HOME/Dropbox/notes/logbook/%Y/%Y-%m-%d.md")
}

logbooks(){
    a=${1:-1}
    b=${2:-1}
    fd . ~/Dropbox/notes/logbook -e md | sort -r | sed -n "$a","$b"p
}

logbook_recent() { # Display the last N logbooks (or from $1 to $2)
    a=${1:-1}
    b=${2:-10}
    bat `logbooks $a $b` --style=header,grid
}
alias lbr="logbook_recent"

logbook_search() { # Display logbooks with contents matching query
    bat $(rg "$@" ~/Dropbox/notes/logbook -l | sort -r) --style=header,grid
}
alias lbs="logbook_search"

nonascii() { # Ripgrep for non-ascii, greek, or "£"
    rg "[^\x00-\x7F£\p{Greek}]" -o --no-heading
}

refresh_dmenu() {
    [ -f ~/.cache/dmenu_run ] && rm ~/.cache/dmenu_run && dmenu_path
}

git_aliases (){
    git config --list | rg alias | column -s '=' -t | sort
}

is_in_git_repo() { 
  git rev-parse HEAD > /dev/null 2>&1
} 

monospace-fonts(){ 
    fc-list :mono | cut -d':' -f2  | cut -d',' -f1 | sort | uniq
} 

duplicates(){ # find duplicate words in a file 
    [[ $# -eq 0 ]] && echo "usage: duplicates <file>..." && return
    grep -Eo '(\b.+) \1\b' $1 || true
} 

to_html() {
    pandoc --standalone --self-contained -c ~/code/dotfiles/simple.css $1 -o $2 
}

due() {
    nlt.py -f "due:%Y-%m-%d" $@
}

tma() {
    tmux attach -t `tmux ls | cut -d':' -f1 | fzf `
}

########################################
#####          EXTERNAL            #####
########################################
external_scripts=(
    $HOME/code/dotfiles/zsh-prompt.sh
    # $HOME/code/dotfiles/functions-notes.sh # ni niv note notes
    $HOME/.envs/py/bin/activate
    $HOME/.cargo/env
    $HOME/.fzf/shell/key-bindings.zsh
    $HOME/code/dotfiles/wsl.sh
    $HOME/.fzf.zsh
)

for external in ${external_scripts[@]}; do
    if [[ -f "$external" ]]; then # if the script exists
        source "$external"
    else
        echo "$external doesn't exist"
    fi
done

# Hide server welcome messages (message of the day, MOTD)
[[ ! -f "$HOME/.hushlogin" ]] && touch "$HOME/.hushlogin"

# cd $HOME

export WASMTIME_HOME="$HOME/.wasmtime"

export PATH="$WASMTIME_HOME/bin:$PATH"

eval "$(zoxide init zsh)"
