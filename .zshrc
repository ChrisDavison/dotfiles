PROMPT="%~ » "
# exports {{{1
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

# add paths to dir, if they exists {{{1
maybe_append_to_path() {
    [[ -d "$1" ]] && export PATH="$1":$PATH
}

maybe_append_to_path $GOBIN
maybe_append_to_path $HOME/bin
maybe_append_to_path $HOME/.bin
maybe_append_to_path $HOME/.vim/pack/plugins/start/fzf/bin/
maybe_append_to_path $HOME/code/scripts/
maybe_append_to_path $HOME/code/scripts/covid/
maybe_append_to_path $HOME/.cargo/bin
maybe_append_to_path $HOME/.local/bin
maybe_append_to_path $HOME/.nimble/bin
maybe_append_to_path /usr/local/go/bin
maybe_append_to_path $HOME/.local/go/bin
maybe_append_to_path $HOME/code/seadas-7.5.3/bin

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

alias ru="repoutil unclean"
alias rs="repoutil stat"
alias rl="repoutil list"
alias rf="repoutil fetch"
alias rb="repoutil branchstat"

alias ls="exa --group-directories-first --git-ignore"
alias lsa="exa --group-directories-first"
alias ll="ls --long --group-directories-first"
alias la="ll -a --group-directories-first"
alias lt="exa --tree -L 2 --group-directories-first"
alias lg="ll --git-ignore"
alias ltg="lt --git-ignore"

alias clip="xclip -sel clipboard"

alias zc="ziputil choose"
alias zv="ziputil view"

alias open="xdg-open"

alias a='fasd -a'        # any
alias s='fasd -si'       # show / search / select
alias d='fasd -d'        # directory
alias f='fasd -f'        # file
alias sd='fasd -sid'     # interactive directory selection
alias sf='fasd -sif'     # interactive file selection
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
alias zz='fasd_cd -d -i' # cd with interactive selection

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

ni(){
    notename="$HOME/code/knowledge/inbox.md"
    echo "- $@" >> $notename
    mdformatwrap $notename
}

niv(){
    bat "$HOME/code/knowledge/inbox.md"
}

note(){
    notename="$HOME/code/knowledge/inbox.md"
    nvim $notename
    mdformatwrap $notename
    clear
    ls
}

notes(){
    pushd $HOME/code/knowledge/
    clear
    ls
}

mdformatwrap(){
    pandoc --to markdown-shortcut_reference_links+pipe_tables-simple_tables-fenced_code_attributes-smart --wrap=auto --columns=72 --atx-headers $1 -o $1
}

tmc(){
    chosen=$(tmux list-sessions 2> /dev/null | cut -d: -f1 | fzf -0)
    [[ -n "$chosen" ]] && tmux attach -t "$chosen"
}


vs(){
    chosen=$(ls -1 ~/.vimsessions | fzf -0)
    [[ -n "$chosen" ]] && nvim -S "~/.vimsessions/$chosen"
}

duplicates(){
    [[ $# -eq 0 ]] && echo "usage: duplicates <file>..." && return
    grep -Eo '(\b.+) \1\b' $1 || true 
}

asmrmpv(){
    nohup mpv $(randomasmr) &> /dev/null &
}

wallpaper(){
    chosen=$(fd . -t f ~/Dropbox/wallpapers | rg -v mobile | fzf)
    [[ -n "$chosen" ]] && feh --bg-fill "$chosen"
}

skyemull() {
    echo "Mull:8811 routed to localhost:9999, via Skye"
    ssh -L 9999:localhost:9999 cdavison@skye ssh -L 9999:localhost:8811 cdavison@mull
}


# up and down do history search
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward

export RANGER_LOAD_DEFAULT_RC=0

# Windows / WSL-specific config
if [[ $(uname -a | grep -q 'Microsoft') -eq 1 ]]; then
    export BROWSER=$(which firefox)
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/.envs/ml/bin/activate
# eval "$(starship init zsh)"
