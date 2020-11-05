set -Ux fish_greeting ""

# Export an env var to declare we are in WSL or not
if not test (uname -r | grep -i -q 'microsoft-standard')
    # grep returns 0 if match is found, so invert result
    set -Ux is_wsl 1
else
    set -Ux is_wsl 0
end



set -Ux GOPATH "$HOME"
set -Ux GOBIN "$HOME/bin"
set -l CARGOBIN "$HOME/.cargo/bin"
set -Ux WORKON_HOME "$HOME/.envs"
set -Ux LESS FRSX
set -Ux CODEDIR "$HOME/code/"
set -Ux VIRTUAL_ENV_DISABLE_PROMPT 0
set -Ux RUST_SRC_PATH "$HOME/.rust_src"
set -Ux RANGER_LOAD_DEFAULT_RC 0

############################################################
for direc in $GOBIN $HOME/.bin $HOME/code/scripts $CARGOBIN /usr/local/go/bin /usr/local/julia/bin $HOME/.local/bin $HOME/.emacs.d/bin $HOME/.npm-packages/bin $HOME/.conda/bin
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
    set -Ux FZF_ALT_C_COMMAND 'fd -t d . $HOME'
    set -Ux FZF_DEFAULT_COMMAND "fd -H -E ".git' -E '.keep' --type file --follow'
else if test -x "$rg_path"
    set -Ux FZF_DEFAULT_COMMAND 'rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
end



############################################################
alias tmux="set TERM xterm-256color; tmux"
alias c="clear"
alias cp="cp -rv" # Always recursively and verbosely copy
alias mv="mv -v" # Always explain move actions
alias mkdir="mkdir -pv" # Always make parent directories, and explain what was done
alias less='less -R' # Use color codes in 'less'
alias rg='rg -S' # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
if test -x "$hub_path"
    alias g="hub"
else
    alias g="git"
end
alias today="date +%F"
alias timestamp="date +'%F %H:%M:%S'"
alias tmux="tmux -2"
alias ts="tagsearch"
alias bm="bookmarks"
alias b="bat --tabs 2 --color=always --style=numbers,changes "
alias n="echo '-  $argv' >> ~/code/knowledge/inbox.txt"
alias nt="echo '-  [ ] $argv' >> ~/code/knowledge/inbox.txt"
alias inbox="nvim ~/code/knowledge/inbox.txt"
alias n="note.py"
alias clip="xclip -sel clipboard"

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
test $is_wsl; and wsl_interop_setup

test -x "$HOME/.envs/ml/bin/activate.fish"; and source "$HOME/.envs/ml/bin/activate.fish"
test -x "$HOME/.envs/py/bin/activate.fish"; and source "$HOME/.envs/py/bin/activate.fish"
test -x "$HOME/.cargo/env"; and source "$HOME/.cargo/env"
test -x "$starship_path"; and starship init fish | source

if test $is_wsl
    set -Ux EDITOR "emacsclient -t"
else
    set -Ux EDITOR "vim"
end

############################################################
if not test -f "$HOME/.hushlogin"
    touch "$HOME/.hushlogin"
    echo "Future server MOTDs will be ignored"
end

cd ~
