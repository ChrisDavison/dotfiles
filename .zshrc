export ZSH="/Users/davison/.oh-my-zsh"
ZSH_THEME="flazz"
DISABLE_AUTO_UPDATE="true"
plugins=( )
source $ZSH/oh-my-zsh.sh

SAVEHIST=100000
HISTSIZE=100000
HISTFILE=~/.zsh_history

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent
setopt auto_param_slash
setopt complete_in_word
setopt glob_complete
setopt list_beep
setopt list_packed
setopt list_rows_first
setopt no_beep
setopt append_history
unsetopt share_history
unsetopt bang_hist
unsetopt extended_history

export TERM=xterm-256color
export EDITOR="nvim"
export GOPATH="$HOME"
export GOBIN="$GOPATH/bin"
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export CODEDIR="$HOME/code"
export TODOFILE="$HOME/Dropbox/notes/todo.md"
export DONEFILE="$HOME/Dropbox/notes/done.md"
export ASMRFILE="$HOME/Dropbox/asmr.csv"
export NOTESDIR="$HOME/Dropbox/notes"
export NOTESBACKUPDIR="$CODEDIR/knowledge"

# =====================
#         Paths
# =====================

path+=$HOME/.vim/bundle/fzf/bin
path+=/usr/local/lib/node_modules
path+=$GOBIN
path+=$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/bin
path+=/Users/davison/Library/Python/3.7/bin/
path+=/Applications/Julia-1.0.app/Contents/Resources/julia/bin/
path+=$CODEDIR/scripts/
path+=$HOME/.cargo/bin/
path+=$HOME/bin
path+=$HOME/.virtualenvs/
path+=/usr/local/miniconda3/bin

# Remove duplicates from $PATH
typeset -aU path

# =====================
#        Aliases
# =====================
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

alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias vi='nvim'
alias tma=choose_tmux_session
alias ipython="ipython --pprint --no-banner"
alias g="git"

# =====================
#        Scripts
# =====================
# ~/bin/randomquote || echo "No bin/randomquote"

source "$CODEDIR/dotfiles/functions.zsh" || echo "No functions.zsh"
source $HOME/.cargo/env || echo "No .cargo/env"
source /usr/local/etc/profile.d/autojump.sh || echo "No autojump.sh"
source ~/.fzf.zsh || echo "No fzf"
source /usr/local/bin/virtualenvwrapper.sh || echo "No virtualenvwrapper"
