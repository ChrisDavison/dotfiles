set -gx fish_greeting ""
set -gx EDITOR "vim"

# Update PATH
# Do this first, so that I can check for binaries later
# e.g. only alias stuff if rust tools like exa or rg exist
not contains $HOME/.bin $PATH; and set PATH $HOME/.bin $PATH
not contains $HOME/code/scripts $PATH; and set PATH $HOME/code/scripts $PATH
not contains $HOME/code/dotfiles/bin $PATH; and set PATH $HOME/code/dotfiles/bin $PATH
not contains $HOME/.cargo/bin $PATH; and set PATH $HOME/.cargo/bin $PATH
not contains $HOME/.local/bin $PATH; and set PATH $HOME/.local/bin $PATH
not contains $HOME/.emacs.d/bin $PATH; and set PATH $HOME/.emacs.d/bin $PATH
not contains $HOME/.npm-packages/bin $PATH; and set PATH $HOME/.npm-packages/bin $PATH
not contains $HOME/.conda/bin $PATH; and set PATH $HOME/.conda/bin $PATH
not contains $HOME/.wasmtime/bin $PATH; and set PATH $HOME/.wasmtime/bin $PATH
not contains $HOME/.cargo/bin $PATH; and set PATH $HOME/.cargo/bin $PATH
not contains /usr/local/go/bin $PATH; and set PATH /usr/local/go/bin $PATH
not contains /usr/local/julia/bin $PATH; and set PATH /usr/local/julia/bin $PATH
not contains /usr/local/zig $PATH; and set PATH /usr/local/zig $PATH

set -gx GOPATH "$HOME"
set -gx GOBIN "$HOME/.bin"
set -gx WORKON_HOME "$HOME/.envs"
set -gx LESS FRSX
set -gx CODEDIR "$HOME/code/"
set -gx VIRTUAL_ENV_DISABLE_PROMPT 0
set -gx RUST_SRC_PATH "$HOME/.rust_src"
set -gx RANGER_LOAD_DEFAULT_RC 0
set -gx RE_UUID "[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}"
set -gx AIRFLOW_HOME "$HOME/.config/airflow"

if not test -z (which fd)
    set -gx FZF_DEFAULT_COMMAND 'fd --type file --hidden --no-ignore'
else
    if not test -z (which fdfind)
        alias fd="fdfind"
        set -gx FZF_DEFAULT_COMMAND 'fd --type file --hidden --no-ignore'
    else
        set -gx FZF_DEFAULT_COMMAND 'rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
    end
end

# color man
set -x LESS_TERMCAP_md (printf "\e[01;31m")
set -x LESS_TERMCAP_me (printf "\e[0m")
set -x LESS_TERMCAP_se (printf "\e[0m")
set -x LESS_TERMCAP_so (printf "\e[01;44;33m")
set -x LESS_TERMCAP_ue (printf "\e[0m")
set -x LESS_TERMCAP_us (printf "\e[01;32m")

set -x MANPAGER "less -R"

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

# some local vars for testing existance of tools
if test -x "$HOME/.bin/nvim.appimage"
    alias v="$HOME/.bin/nvim.appimage"
    set -gx EDITOR "$HOME/.bin/nvim.appimage"
end

alias g="git"
not test -z (which hub); and alias g="hub"

not test -z (which fdfind); and alias fd="fdfind"

if not test -z (which repoutil)
    alias ru="repoutil unclean"
    alias rs="repoutil stat"
    alias rl="repoutil list"
    alias rf="repoutil fetch"
    alias rb="repoutil branchstat | sed -e 's/.*code\///' | sort | column -s'|' -t"
else
    echo "repoutil not installed"
end

if not test -z (which exa)
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

if not test -z (which ziputil)
    alias zc="ziputil choose"
    alias zv="ziputil view"
else
    echo "ziputil not installed"
end

if not test -z (which gh)
    gh completion -s fish | source
end


# Source python environment
test -f "$HOME/.envs/py/bin/activate.fish"; and source "$HOME/.envs/py/bin/activate.fish"

# Source starship for a more informative
# starship is a bit buggy within emacs (term, rather than vterm)
# so commenting out for now
# test -f "$HOME/.cargo/bin/starship"; and starship init fish | source
# test -f "/usr/local/bin/starship"; and starship init fish | source

# Ignore server login messages
not test -f "$HOME/.hushlogin"; and touch "$HOME/.hushlogin"

# WASM config
set -l WASMTIME_HOME "$HOME/.wasmtime"
string match -r ".wasmtime" "$PATH" >/dev/null; or set -gx PATH "$WASMTIME_HOME/bin" $PATH

and zoxide init fish | source

# Setup only for WSL (linux on windows)
not test (uname -r | grep -i 'microsoft'); and wsl_interop_setup
