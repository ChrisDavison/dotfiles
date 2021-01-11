####### #     # #     #       #     #    #    ######   #####
#       ##    # #     #       #     #   # #   #     # #     #
#       # #   # #     #       #     #  #   #  #     # #
#####   #  #  # #     #       #     # #     # ######   #####
#       #   # #  #   #         #   #  ####### #   #         #
#       #    ##   # #           # #   #     # #    #  #     #
####### #     #    #             #    #     # #     #  #####
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

paths=($HOME/bin $HOME/.bin $HOME/.fzf/bin $HOME/code/scripts $HOME/.cargo/bin $HOME/.local/bin $HOME/.nimble/bin $HOME/.local/go/bin /usr/local/go/bin)
for p in ${paths[@]}; do
    export PATH="$p":$PATH
done
typeset -U path
typeset -U PATH

 #####  ####### ####### ####### ### #     #  #####   #####
#     # #          #       #     #  ##    # #     # #     #
#       #          #       #     #  # #   # #       #
 #####  #####      #       #     #  #  #  # #  ####  #####
      # #          #       #     #  #   # # #     #       #
#     # #          #       #     #  #    ## #     # #     #
 #####  #######    #       #    ### #     #  #####   #####
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

external_scripts=(
    $HOME/code/dotfiles/zsh-prompt.sh
    $HOME/code/dotfiles/aliases
    $HOME/code/dotfiles/functions
    # $HOME/code/dotfiles/functions-notes.sh # ni niv note notes
    $HOME/code/dotfiles/functions-git-fzf.sh # fco, fshow, fgst
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
