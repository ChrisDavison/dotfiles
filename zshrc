setopt correctall
# ===================================
#                ALIASES
# ===================================
# Aliases for bash, zsh etc

# Directory listing aliases

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

# Session management
alias c='clear'
alias ..='cd ..'

# Copy SSH public key
alias cbssh="cbf ~/.ssh/id_rsa.pub"

# Python machine-learning environment
alias datasci="source ~/.envs/datasci/bin/activate"

# Utilities
alias cl='catless'
alias less='less -R'

# Make ripgrep Smart-case search by default
alias rg='rg -S'

alias ME='cd ~/src/github.com/chrisdavison'
alias CHURN='cd ~/src/github.com/etsteam/churning'

alias nbx="jupyter nbconvert --execute --to notebook"

# Docker bollocks
alias batrem='pmset -g batt | rg -o "\d+:\d+ remaining"'

# Git multirepo assistance
alias g='git'
alias dr='gr status | grep -E "behind|ahead|modified"'
alias gitsync='gr git fetch --all'
alias gitdown='gr git pull --rebase'

gg(){
    git grep -a -i "$1"
}

gga(){
    git grep -a -i "$1" $(git rev-list --all)
}


choose_tmux_session() {
    if tmux list-sessions 2>&1 >> /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
}
alias tma=choose_tmux_session
alias tmuxhere='tmux new -s $(basename $(pwd))'

# ===================================
#                EXPORTS
# ===================================
# Exports for bash/zsh
[[ -x /usr/local/bin/nvim ]] && export EDITOR="nvim" || export EDITOR="vim"

# Terminal history handling
export HISTCONTROL=ignoreboth:erasedups;
export HISTSIZE=32768;
export HISTFILE=~/.zsh_history
export HISTFILESIZE=$HISTSIZE;
export HISTFILESIZE=2000;
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help";
export SAVEHIST=9000

# Environment variables/path
export GOPATH="$HOME";
export GOBIN="$HOME/bin";
export TEXPATH="/usr/texbin";
export NODEPATH="/usr/local/lib/node_modules";
export HOMEBIN="$GOBIN"
export MULTIRUSTBIN="$HOME/.multirust/toolchains/nightly/cargo/bin"
export STACKBIN="$HOME/.local/bin"
export RESEARCHFIGURES="$HOME/Dropbox/f/figures/"

export PATH=$GOBIN:$TEXBIN:$NODEPATH:$HOMEBIN:$MULTIRUSTBIN:$STACKBIN:$PATH;

# 256 color terminal
export TERM=xterm-256color;

export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3

# Export various directories useful for mu work
export CHURNING_DATA_DIR="$HOME/.data/"
export LOGBOOK_DIR="$HOME/src/github.com/chrisdavison/logbook/"

choose_vim() {
    selected=$(find ~/.vim-sessions -name "*.vim" -type f | fzf -q "$1")
    [[ -n "$selected" ]] && $EDITOR -S "$selected"
}
alias cv='choose_vim'

# ===================================
#         SOURCE CUSTOM SCRIPTS
# ===================================
sourceOrErrorMessage() {
    [ -f $1 ] && source $1 || echo "No $1"
}
sourceOrErrorMessage ~/.dotfiles/setopt.sh      # from zanshin
sourceOrErrorMessage ~/.dotfiles/completion.sh  # from zanshin

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

# ===================================
#                 PROMPT
# ===================================
local prompt_string="⌁"

# Make prompt_string red if the previous command failed.
local return_status="%(?:%{%F{green}%}$prompt_string:%{%F{red}%}$prompt_string)"

NEWLINE=$'\n'
MEANDDIR="(%n@%m:%~)"
PROMPTTIME="[20%D %t]"
PROMPT="%{%F{green}%}${PROMPTTIME} ${MEANDDIR}${NEWLINE}# %F{reset}%}"

# ===================================
#              KEY BINDING
# ===================================
# To see the key combo you want to use just do:
# cat > /dev/null
# And press it

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


# ===================================
#       SOURCE INSTALLED SCRIPTS
# ===================================
sourceOrErrorMessage ~/.cargo/env
sourceOrErrorMessage ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
sourceOrErrorMessage /usr/local/etc/profile.d/autojump.sh

PATH="/Users/davison/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/davison/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/davison/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/davison/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/davison/perl5"; export PERL_MM_OPT;

as_md_anchor(){
    input="$@"
    echo "{#$(slugify $@)}"
}