set -gx fish_greeting ""
set -gx EDITOR "vim"

# Export an env var to declare we are in WSL or not
if not test (uname -r | grep -i -q 'microsoft-standard')
    # grep returns 0 if match is found, so invert result
    set -gx is_wsl 1
    wsl_interop_setup
else
    set -gx is_wsl 0
end

set -gx GOPATH "$HOME"
set -gx GOBIN "$HOME/bin"
set -l CARGOBIN "$HOME/.cargo/bin"
set -gx WORKON_HOME "$HOME/.envs"
set -gx LESS FRSX
set -gx CODEDIR "$HOME/code/"
set -gx VIRTUAL_ENV_DISABLE_PROMPT 0
set -gx RUST_SRC_PATH "$HOME/.rust_src"
set -gx RANGER_LOAD_DEFAULT_RC 0
set -gx RE_UUID "[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}"

############################################################
for direc in $GOBIN $HOME/.bin $HOME/code/scripts $CARGOBIN /usr/local/go/bin /usr/local/julia/bin $HOME/.local/bin $HOME/.emacs.d/bin $HOME/.npm-packages/bin $HOME/.conda/bin /usr/local/zig
    if not contains $direc $PATH
        set PATH $direc $PATH
    end
end

############################################################

# some local vars for testing existance of tools
set -l rg_path (which rg)
set -l fd_path (which fd)
set -l fdfind_path (which fdfind)
set -l exa_path (which exa)
set -l repoutil_path (which repoutil)
set -l ziputil_path (which ziputil)
set -l hub_path (which hub)
set -l starship_path (which starship)

if test -x "$fd_path" -o -x "$fdfind_path"
    set -gx FZF_ALT_C_COMMAND 'fd -t d . $HOME'
    set -gx FZF_DEFAULT_COMMAND "fd -H -E ".git' -E '.keep' --type file --follow'
else if test -x "$rg_path"
    set -gx FZF_DEFAULT_COMMAND 'rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
end

############################################################
alias b="bat --tabs 2 --color=always --style=numbers,changes "
alias bm="bookmarks"
alias clip="xclip -sel clipboard"
alias cp="cp -rv" # Always recursively and verbosely copy
alias df="df -x squashfs"
alias ipython="ipython --pprint --no-banner"
alias less='less -R' # Use color codes in 'less'
alias mkdir="mkdir -pv" # Always make parent directories, and explain what was done
alias mv="mv -v" # Always explain move actions
alias rg='rg -S' # Make ripgrep use smart-case by default
alias timestamp="date +'%F %H:%M:%S'"
alias today="date +%F"
alias ts="tagsearch"
alias l7w="last_work_week"
alias l7j="last_journal_week"
alias dls="cat ~/.download"
alias dlaq="dla -q"

alias v="vim"
if test -x "$HOME/.bin/nvim.appimage"
    alias v="$HOME/.bin/nvim.appimage"
    set -gx EDITOR "$HOME/.bin/nvim.appimage"
end

alias g="git"
if test -x "$hub_path"
    alias g="hub"
end

if test -x "$fdfind_path"
    alias fd="fdfind"
end

if test -x "$repoutil_path"
    alias ru="repoutil unclean"
    alias rs="repoutil stat"
    alias rl="repoutil list"
    alias rf="repoutil fetch"
    alias rb="repoutil branchstat | sed -e 's/.*code\///' | sort | column -s'|' -t"
else
    echo "repoutil not installed"
end

if test -x "$exa_path"
    alias ls="exa --group-directories-first"
    alias lsa="exa --group-directories-first"
    alias ll="ls --long --group-directories-first"
    alias la="ll -a --group-directories-first"
    alias lt="exa --tree -L 2 --group-directories-first"
    alias lg="ll --git-ignore"
    alias ltg="lt --git-ignore"
else
    echo "exa not installed. install from cargo"
end

if test -x "$ziputil_path"
    alias zc="ziputil choose"
    alias zv="ziputil view"
else
    echo "ziputil not installed"
end

############################################################

source "$HOME/.envs/py/bin/activate.fish"
source "$HOME/.cargo/env"

test -x "$starship_path"; and starship init fish | source


############################################################
if not test -f "$HOME/.hushlogin"
    touch "$HOME/.hushlogin"
    echo "Future server MOTDs will be ignored"
end

cd ~

# WASM config
set -l WASMTIME_HOME "$HOME/.wasmtime"
string match -r ".wasmtime" "$PATH" > /dev/null; or set -gx PATH "$WASMTIME_HOME/bin" $PATH

if test -x (which zoxide)
    zoxide init fish | source
end
