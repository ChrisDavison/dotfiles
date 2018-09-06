if [ -x "$(command -v gls)" ]; then
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

alias datasci="source ~/.envs/datasci/bin/activate"     # Source my common python environment
alias less='less -R'    # Use color codes in 'less'
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias g='git'
alias dr='gr status | grep -E "behind|ahead|modified"'
alias gitsync='gr git fetch --all'
alias gitdown='gr git pull --rebase'
alias git-root='cd $(git rev-parse --show-toplevel)'
alias vi='mvim -v'

# KEY BINDING =============================================
bindkey "^K"      kill-whole-line                      # ctrl-k
bindkey "^R"      history-incremental-search-backward  # ctrl-r
bindkey "^A"      beginning-of-line                    # ctrl-a
bindkey "^E"      end-of-line                          # ctrl-e
bindkey "[B"      history-search-forward               # down arrow
bindkey "[A"      history-search-backward              # up arrow
bindkey "^D"      delete-char                          # ctrl-d
bindkey "^F"      forward-char                         # ctrl-f
bindkey "^B"      backward-char                        # ctrl-b
bindkey -v   # Default to standard vi bindings, regardless of editor string


# FUNCTIONS ===============================================
cv() {
    if [ -d ~/.vim-sessions ]; then
        selected=$(find ~/.vim-sessions -name "*.vim" -type f | fzf -q "$1")
        [[ -n "$selected" ]] && $EDITOR -S "$selected"
    else
        echo "Couldn't find ~/.vim-sessions folder"
    fi
}

choose_tmux_session() {
    if tmux list-sessions 2>&1 > /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
}
alias tma=choose_tmux_session

# EXPORTS ================= ===============================
export EDITOR="vim"
# Terminal history handling ===============================
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=32768
export HISTFILE=~/.zsh_history
export HISTFILESIZE=$HISTSIZE
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"
export SAVEHIST=$HISTSIZE

# PATH  ===================================================
export GOPATH="$HOME"
export GOBIN="$HOME/bin"
export TEXPATH="/usr/texbin"
export NODEPATH="/usr/local/lib/node_modules"
export HOMEBIN="$GOBIN"
export MULTIRUSTBIN="$HOME/.multirust/toolchains/nightly/cargo/bin"
export JULIAPATH="/Applications/Julia-1.0.app/Contents/Resources/julia/bin/"
export PATH=$JULIAPATH:$GOBIN:$TEXBIN:$NODEPATH:$HOMEBIN:$MULTIRUSTBIN:$STACKBIN:$PATH;

export TERM=xterm-256color;

export LOGBOOK_DIR="$HOME/src/github.com/chrisdavison/logbook/"
export RESEARCHFIGURES="$HOME/Dropbox/work/figures/"

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

# SOURCE EXTERNAL STUFF ===================================
sourceOrErrorMessage() {
    [ -f $1 ] && source $1 || echo "No $1"
}
sourceOrErrorMessage ~/.cargo/env
sourceOrErrorMessage ~/.fzf.zsh
sourceOrErrorMessage /usr/local/etc/profile.d/autojump.sh

# PROMPT ==================================================
PROMPTDIR="%~"
NAMEANDHOST="(%n@%m)"
PROMPTCHAR="."
PROMPT="%{%F{green}%}${PROMPTDIR} ${PROMPTCHAR} %F{reset}%}"
