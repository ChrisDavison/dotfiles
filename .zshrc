DOTFILES=$HOME/code/dotfiles

# My own dotfiles
source $DOTFILES/shell/checks.sh
source $DOTFILES/shell/exports.sh
source $DOTFILES/shell/path.sh
source $DOTFILES/shell/setopt.sh
source $DOTFILES/shell/completion.sh
source $DOTFILES/shell/keybinds.sh
source $DOTFILES/shell/aliases.sh
source $DOTFILES/shell/functions.sh
source $DOTFILES/shell/prompt.sh

[[ "$IS_WSL" -eq 1 ]] && source $DOTFILES/wsl.sh

# Other programs
[[ -f $HOME/.envs/py/bin/activate ]] && source $HOME/.envs/py/bin/activate
[[ -f $HOME/.cargo/env ]] && source $HOME/.cargo/env
[[ -f $HOME/.fzf/shell/key-bindings.zsh ]] && source $HOME/.fzf/shell/key-bindings.zsh
[[ -f $HOME/.fzf.zsh ]] && source $HOME/.fzf.zsh

# Hide server welcome messages
if [[ ! -f "$HOME/.hushlogin" ]]; then
    touch "$HOME/.hushlogin"
fi

[[ -x $(which zoxide) ]] && eval "$(zoxide init zsh)"

