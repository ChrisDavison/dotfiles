set -Ux fish_greeting ""

set -Ux EDITOR "emacsclient -t"
set -Ux GOPATH "$HOME"
set -Ux GOBIN "$HOME/bin"
set -Ux FZF_DEFAULT_COMMAND 'rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
set -Ux FZF_ALT_C_COMMAND 'fd -t d . $HOME'
set -Ux WORKON_HOME "$HOME/.envs"
set -Ux LESS FRSX
set -Ux CODEDIR "$HOME/code/"
set -Ux VIRTUAL_ENV_DISABLE_PROMPT 0
set -Ux RUST_SRC_PATH "$HOME/.rust_src"

set -Ux RANGER_LOAD_DEFAULT_RC 0

if not uname -a | grep -q "Microsoft"
    set -Ux BROWSER "firefox"
end

for direc in $GOBIN $HOME/.bin $HOME/code/scripts $HOME/.cargo/bin /usr/local/go/bin /usr/local/julia/bin $HOME/.local/bin $HOME/.emacs.d/bin $HOME/.npm-packages/bin
    if not contains $direc $PATH
        set PATH $direc $PATH
    end
end

alias tmux="set TERM xterm-256color; tmux"
alias c="clear"
alias cp="cp -rv" # Always recursively and verbosely copy
alias mv="mv -v" # Always explain move actions
alias mkdir="mkdir -pv" # Always make parent directories, and explain what was done
alias less='less -R' # Use color codes in 'less'
alias rg='rg -S' # Make ripgrep use smart-case by default
alias v="$EDITOR"
alias ipython="ipython --pprint --no-banner"
alias g="git"
test -e (which hub); and alias g="hub"
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
alias rb="repoutil branchstat | sed -e 's/.*code\///' | sort | column -s'|' -t"

alias fd="fdfind"
alias ls="exa --group-directories-first"
alias lsa="exa --group-directories-first"
alias ll="ls --long --group-directories-first"
alias la="ll -a --group-directories-first"
alias lt="exa --tree -L 2 --group-directories-first"
alias lg="ll --git-ignore"
alias ltg="lt --git-ignore"

alias clip="xclip -sel clipboard"

alias zc="ziputil choose"
alias zv="ziputil view"

test -x rvm; and rvm default

uname -r | rg -i -q 'microsoft-standard' >/dev/null
if test $status -eq 0
    set -Ux DISPLAY (grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0
    set -Ux LIBGL_ALWAYS_INDIRECT 1

    set -Ux NO_AT_BRIDGE 1
    for i in (pstree -np -s $fish_pid | grep -o -E '[0-9]+')
        if ! test -e "/run/WSL/$i_interop"
            set -x WSL_INTEROP /run/WSL/$i_interop
            echo $WSL_INTEROP >~/.wsl_interop
        end
    end
end

# test -f ~/.envs/ml/bin/activate.fish; and source ~/.envs/ml/bin/activate.fish

starship init fish | source

cd ~
