PROMPT="%~ » "
# exports {{{1
export EDITOR="nvim"
export GOPATH="$HOME"
export GOBIN="$HOME/bin"
export FZF_DEFAULT_COMMAND='rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
export FZF_ALT_C_COMMAND='fd -t d . $HOME'
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export VIRTUAL_ENV_DISABLE_PROMPT=1

# add paths to dir, if they exists {{{1
export_to_path_if_exists() {
    [[ -d "$1" ]] && export PATH="$1":$PATH
}

export_to_path_if_exists $GOBIN
export_to_path_if_exists $HOME/bin
export_to_path_if_exists $HOME/.bin
export_to_path_if_exists $HOME/.vim/pack/plugins/start/fzf/bin/
export_to_path_if_exists $HOME/code/scripts/
export_to_path_if_exists $HOME/.cargo/bin
export_to_path_if_exists $HOME/.local/bin
export_to_path_if_exists $HOME/.nimble/bin
export_to_path_if_exists /usr/local/go/bin
export_to_path_if_exists /snap/bin

[[ -x rvm ]] && rvm default

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
setopt auto_list # automatically list choices on ambiguous completion
setopt auto_menu # automatically use menu completion
setopt always_to_end # move cursor to end if word had one match
setopt no_beep #turn off terminal bell
setopt extended_glob

# completion
zstyle ':completion:*' menu select # select completions with arrow keys
zstyle ':completion:*' group-name '' # group results by category
zstyle ':completion:::::' completer _expand _complete _ignored _approximate #enable approximate matches for completion

autoload -Uz compinit;compinit -i

# aliases {{{1
alias tmux="set TERM xterm-256color; tmux -2"
alias c="clear"
alias cp="cp -rv"    # Always recursively and verbosely copy
alias mv="mv -v"     # Always explain move actions
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
alias g="git"
if [[ -x $HOME/bin/hub ]] || [[ -x $HOME/.bin/hub ]]; then
    alias g="hub"
fi
alias today="date +%F"
alias ts="tagsearch"
alias clip="xclip -sel clipboard"
alias n="note.py"
# keybinds
autoload -U history-search-end
bindkey -e
bindkey "^[[A" history-beginning-search-backward-end
bindkey "^[[B" history-beginning-search-forward-end
# don't let '^d' exit the shell if there's nothing on the line
stty eof undef
#     Repoutil {{{1
alias ru="repoutil unclean"
alias rs="repoutil stat"
alias rl="repoutil list"
alias rf="repoutil fetch"
alias rb="repoutil branchstat"
# ls (exa)
alias ls="exa --group-directories-first --git-ignore"
alias lsa="exa --group-directories-first"
alias ll="ls --long --group-directories-first"
alias la="ll -a --group-directories-first"
alias lt="exa --tree -L 2 --group-directories-first"
alias lg="ll --git-ignore"
alias ltg="lt --git-ignore"

alias b="bat --tabs 2 --color=always --style=numbers,changes "


#     Jump to recent directories {{{1
alias d='dirs -v | head -10'
alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

# source other scripts {{{1
function source_if_exists {
    [[ -f "$1" ]] && source "$1"
    [[ $VERBOSE_ZSHRC -eq 1 ]] && echo "Sourced" "$1"
}
source_if_exists $HOME/.cargo/env
source_if_exists $HOME/.vim/pack/plugins/start/fzf/shell/key-bindings.zsh

# fzf functions {{{1
# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

fshow(){
    local commit commits
    commits=$(git log --oneline) &&
        commit=$(echo "$commits" | fzf --preview 'git show --abbrev-commit --stat --color=always $(echo {} | cut -d" " -f1)') &&
    git checkout $(echo "$branch" | cut -d' ' -f1)
}
zle -N fshow
bindkey '^gc' fshow

fco() {
  local tags branches target
  branches=$(
    git --no-pager branch --all \
      --format="%(if)%(HEAD)%(then)%(else)%(if:equals=HEAD)%(refname:strip=3)%(then)%(else)%1B[0;34;1mbranch%09%1B[m%(refname:short)%(end)%(end)" \
    | sed '/^$/d') || return
  tags=$(
    git --no-pager tag | awk '{print "\x1b[35;1mtag\x1b[m\t" $1}') || return
  target=$(
    (echo "$branches"; echo "$tags") |
    fzf --no-hscroll --no-multi -n 2 \
        --ansi) || return
  git checkout $(awk '{print $2}' <<<"$target" )
}
zle -N fco
bindkey '^gb' fco

# fgst - pick files from `git status -s`
is_in_git_repo() {
  git rev-parse HEAD > /dev/null 2>&1
}

fgst() {
  # "Nothing to see here, move along"
  is_in_git_repo || return 1

  local cmd="${FZF_CTRL_T_COMMAND:-"command git status -s"}"

  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" fzf -m "$@" | while read -r item; do
    echo "$item" | awk '{print $2}'
  done
  echo
}
alias vfg='nvim $(fgst)'

# functions {{{1
sanitise(){
    [[ $# -eq 0 ]] && echo "usage: sanitise <filename>" && return
    direc=$(dirname $1)
    base=$(basename $1)
    echo "$base" |tr '[:upper:]' '[:lower:]' | sed 's/[^a-zA-Z0-9.-]/-/g' | tr -s - - | sed 's/\-$//g'
}

ppath(){
    echo "$PATH" | tr ":" "\n"
}

monospace-fonts(){
	fc-list :mono | cut -d':' -f2  | cut -d',' -f1 | sort | uniq
}

aliases(){
    grep "^\s*alias.*=" ~/.zshrc | sed -e 's/^[ ]\+//g' | column -s '=' -t | cut -d' ' -f2-
}

nonascii(){
    rg "[^\x00-\x7F£\p{Greek}]" -o --no-heading $@
}

is_tmux_alive(){
    if [ $(tmux list-sessions | wc -l) -gt 0 ];
    then
        echo "TMUX"
    else
        echo "noMUX"
        fi
}

datezipdir(){
    [[ $# -eq 0 ]] && echo "usage: datezipdir <directory>" && return
    dirname=$(basename $1)
    zipname=$(date +"$dirname--%Y-%m-%d.zip")
    echo $zipname
    zip -r $zipname $1
}

aesenc(){
    [[ $# = 0 ]] && echo "usage: aesenc <file>" && return
    gpg --symmetric -a --cipher-algo aes256 --output "$1".asc "$1"
    echo "$1.asc created"
}

# find duplicate words
duplicate_words(){
    [[ $# -eq 0 ]] && echo "usage: duplicate_words <file>..." && return
    grep -Eo '(\b.+) \1\b' $1 || true
}

# up and down do history search
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward

export RANGER_LOAD_DEFAULT_RC=0

# Windows / WSL-specific config
if [[ $(uname -a | grep -q 'Microsoft') -eq 0 ]]; then
    export BROWSER=/mnt/c/Program\ Files/Mozilla\ Firefox/firefox.exe
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
