setopt correctall

dotfiles_dir=~/.dotfiles

sourceOrErrorMessage() {
    [ -f $1 ] && source $1 || echo "No $1"
}

sourceOrErrorMessage ${dotfiles_dir}/aliases.sh
sourceOrErrorMessage ${dotfiles_dir}/keybinds.sh
sourceOrErrorMessage ${dotfiles_dir}/funcs.sh
sourceOrErrorMessage ${dotfiles_dir}/exports.sh
# From zanshin
sourceOrErrorMessage ${dotfiles_dir}/setopt.sh
sourceOrErrorMessage ${dotfiles_dir}/completion.sh
sourceOrErrorMessage ~/.cargo/env
sourceOrErrorMessage ~/.fzf.zsh
sourceOrErrorMessage /usr/local/etc/profile.d/autojump.sh

# ===================================
#                 PROMPT
# ===================================
PROMPTDIR="%~"
NAMEANDHOST="(%n@%m)"
PROMPTCHAR="."
PROMPT="%{%F{green}%}${PROMPTDIR} ${PROMPTCHAR} %F{reset}%}"
