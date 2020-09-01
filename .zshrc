PROMPT="%~ » "
# exports / environment variables {{{1
export EDITOR="nvim"
export GOPATH="$HOME"
export GOBIN="$HOME/bin"
# export FZF_DEFAULT_COMMAND='rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
export FZF_DEFAULT_COMMAND="fd -H -E '.git' -E '.keep' --type file --follow"
export FZF_ALT_C_COMMAND='fd -t d . $HOME'
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export VIRTUAL_ENV_DISABLE_PROMPT=1
export MAIL=~/.mbox
export TODOFILE=~/Dropbox/todo.txt
export DONEFILE=~/Dropbox/done.txt
export RE_UUID="[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}"
export RANGER_LOAD_DEFAULT_RC=0
export RUST_SRC_PATH="$HOME/.rust_src"

# add paths to dir, if they exists {{{1
maybe_append_to_path() {
    [[ -d "$1" ]] && export PATH="$1":$PATH
}

maybe_append_to_path $GOBIN
maybe_append_to_path $HOME/bin
maybe_append_to_path $HOME/.bin
maybe_append_to_path $HOME/.fzf/bin/
maybe_append_to_path $HOME/code/scripts/
maybe_append_to_path $HOME/code/scripts/covid/
maybe_append_to_path $HOME/.cargo/bin
maybe_append_to_path $HOME/.local/bin
maybe_append_to_path $HOME/.nimble/bin
maybe_append_to_path /usr/local/go/bin
maybe_append_to_path $HOME/.local/go/bin
maybe_append_to_path $HOME/code/seadas-7.5.3/bin
# }}}1
# settings {{{1
setopt  autocd autopushd pushdignoredups

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

# aliases {{{1
alias tmux="set TERM xterm-256color; tmux"
alias c="clear"
alias cp="cp -rv"    # Always recursively and verbosely copy
alias mv="mv -v"     # Always explain move actions
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
alias g="git"
[[ -e $(which hub) ]] && alias g="hub"
alias today="date +%F"
alias tmux="tmux -2"
alias ts="tagsearch"
alias bm="bookmarks"
alias b="bat --tabs 2 --color=always --style=numbers,changes "
alias n="echo '-  $argv' >> ~/code/knowledge/inbox.txt"
alias nt="echo '-  [ ] $argv' >> ~/code/knowledge/inbox.txt"
alias inbox="nvim ~/code/knowledge/inbox.txt"
alias n="note.py"
alias clip="xclip -sel clipboard"
alias df="df -x squashfs"
alias clip="xclip -sel clipboard"
alias zc="ziputil choose"
alias zv="ziputil view"
alias open="xdg-open"
alias j="nvim ~/Dropbox/notes/journal.md"
alias viml="vim -S ~/.lastsession.vim"
# }}}1

# aliases (conditional) {{{1
if type repoutil > /dev/null; then
    alias ru="repoutil unclean"
    alias rs="repoutil stat"
    alias rl="repoutil list"
    alias rf="repoutil fetch"
    alias rb="repoutil branchstat"
else
    echo "repoutil not installed"
fi

if type exa > /dev/null; then
    alias ls="exa --group-directories-first --git-ignore"
    alias lsa="exa --group-directories-first"
    alias ll="ls --long --group-directories-first"
    alias la="ll -a --group-directories-first"
    alias lt="exa --tree -L 2 --group-directories-first"
    alias lg="ll --git-ignore"
    alias ltg="lt --git-ignore"
else
    echo "exa not installed"
fi
# }}}1

# keybinds {{{1
# up and down do history search
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
# }}}1

#===========================================================
# FUNCTIONS
#===========================================================
is_in_git_repo() { # {{{1
  git rev-parse HEAD > /dev/null 2>&1
} # }}}1

sanitise(){ # {{{1
    [[ $# -eq 0 ]] && echo "usage: sanitise <filename>" && return
    direc=$(dirname $1)
    base=$(basename $1)
    echo "$base" |tr '[:upper:]' '[:lower:]' | sed 's/[^a-zA-Z0-9.-]/-/g' | tr -s - - | sed 's/\-$//g'
} # }}}1

ppath(){ # {{{1
    echo "$PATH" | tr ":" "\n"
} # }}}1

monospace-fonts(){ # {{{1
	fc-list :mono | cut -d':' -f2  | cut -d',' -f1 | sort | uniq
} # }}}1

aliases(){ # {{{1
    grep "^\s*alias.*=" ~/.zshrc | sed -e 's/^[ ]\+//g' | column -s '=' -t | cut -d' ' -f2-
} # }}}1

nonascii(){ # {{{1
    rg "[^\x00-\x7F£\p{Greek}]" -o --no-heading $@
} # }}}1

is_tmux_alive(){ # {{{1
    if [ $(tmux list-sessions | wc -l) -gt 0 ];
    then
        echo "TMUX"
    else
        echo "noMUX"
        fi
} # }}}1

datezipdir(){ # {{{1
    [[ $# -eq 0 ]] && echo "usage: datezipdir <directory>" && return
    dirname=$(basename $1)
    zipname=$(date +"$dirname--%Y-%m-%d.zip")
    echo $zipname
    zip -r $zipname $1
} # }}}1

aesenc(){ # {{{1
    [[ $# = 0 ]] && echo "usage: aesenc <file>" && return
    gpg --symmetric -a --cipher-algo aes256 --output "$1".asc "$1"
    echo "$1.asc created"
} # }}}1

mdformatwrap(){ # format markdown using pandoc {{{1
    pandoc --to markdown-shortcut_reference_links+pipe_tables-simple_tables-fenced_code_attributes-smart --wrap=auto --columns=72 --atx-headers $1 -o $1
} # }}}1

tmc(){ # fuzzy choose a tmux session {{{1
    chosen=$(tmux list-sessions 2> /dev/null | cut -d: -f1 | fzf -0)
    [[ -n "$chosen" ]] && tmux attach -t "$chosen"
} # }}}1

vs(){ # fuzzy select a vim session {{{1
    chosen=$(ls -1 ~/.vimsessions | fzf -0)
    [[ -n "$chosen" ]] && nvim -S "~/.vimsessions/$chosen"
} # }}}1

duplicates(){ # find duplicate words in a file {{{1
    [[ $# -eq 0 ]] && echo "usage: duplicates <file>..." && return
    grep -Eo '(\b.+) \1\b' $1 || true
} # }}}1

asmrmpv(){ # launch a random asmr video using mpv {{{1
    nohup mpv $(randomasmr) &> /dev/null &
} # }}}1

wallpaper(){ # set linux wallpaper using feh {{{1
    chosen=$(fd . -t f ~/Dropbox/wallpapers | rg -v mobile | fzf)
    [[ -n "$chosen" ]] && feh --bg-fill "$chosen"
} # }}}1

skyemull() { # alias to ssh to mull, via skye, with tunneling {{{1
    echo "Mull:8811 routed to localhost:9999, via Skye"
    ssh -L -T 9999:localhost:9999 cdavison@skye ssh -T -L 9999:localhost:8811 cdavison@mull
} # }}}1

mull() { # alias to ssh to mull with tunneling {{{1
    echo "Mull:8811 routed to localhost:9999, via Skye"
    ssh mull -L 9999:localhost:8811
} # }}}1

fzs(){ # make a string 'fuzzy', for searching {{{1
    echo "$@" | sed -e 's/\s/.*/g'
} # }}}1

fzfp(){ # fzf with preview {{{1
    fzf --preview="bat {}" --preview-window=right:70%:wrap
} # }}}1

alias eme='
export DISPLAY=$(grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0
export LIBGL_ALWAYS_INDIRECT=1
setsid emacs'

alias emee='
export DISPLAY=$(grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0
export LIBGL_ALWAYS_INDIRECT=1
setsid emacs
exit'

# Windows / WSL-specific config {{{1
if [[ $(uname -a | grep -i -q 'Microsoft') -eq 1 ]]; then
    export BROWSER=$(which firefox)
    export DISPLAY=$(grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0
fi
# }}}1

# SOURCE scripts and plugins {{{1
source_if_exists(){
    [[ -f "$1" ]] && source "$1"
    [[ $VERBOSE_ZSHRC -eq 1 ]] && echo "Sourced" "$1"
}

# fco - fuzzy checkout [C-g b]
# fshow - fuzzy show [C-g c]
# fgst - fuzzy select from git status (vfg - vim, fuzzy from status)
source $HOME/code/dotfiles/functions-git-fzf.sh

# ni - append $@ to inbox
# niv - edit inbox in vim
# note - edit inbox in vim
# notes - go to notes dir and ls
source $HOME/code/dotfiles/functions-notes.sh

source_if_exists ~/.envs/ml/bin/activate
source_if_exists ~/code/dotfiles/zsh-prompt.sh
source_if_exists $HOME/.cargo/env
source_if_exists $HOME/.fzf/shell/key-bindings.zsh
source_if_exists ~/.fzf.zsh

# }}}1

[[ -f $HOME/.servername ]] && echo "On server: $(cat $HOME/.servername)"

if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi
