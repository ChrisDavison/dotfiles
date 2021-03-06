alias bm="bookmarks"
alias clip="xclip -sel clipboard"
alias cp="cp -rv"    # Always recursively and verbosely copy
alias df="df -x squashfs"
alias dls="cat ~/.download"
alias g="git"
alias inbox="nvim $HOME/code/knowledge/inbox.txt"
alias ipython="ipython --pprint --no-banner"
alias less='less -R'    # Use color codes in 'less'
alias mkdir="mkdir -pv"   # Always make parent directories, and explain what was done
alias mv="mv -v"     # Always explain move actions
alias rg='rg -S'   # Make ripgrep use smart-case by default
alias rgl="rg --multiline --multiline-dotall"
alias sz="source ~/.zshrc"
alias tmux="TERM=xterm-256color tmux -2"
alias today="date +%F"
alias ts="tagsearch"
alias v="nvim"

# alias t="todo.sh"
# alias tt="todo.sh due"
# alias tw="todo.sh due @work"

# alias n="echo '-  $argv' >> $HOME/code/knowledge/inbox.txt"
# alias nt="echo '-  [ ] $argv' >> $HOME/code/knowledge/inbox.txt"
# alias n="note.py"

[[ -x "$HOME/.bin/nvim.appimage" ]] && alias v="$HOME/.bin/nvim.appimage"

# aliases (conditional)
 
# fd was installed from apt, so is installed as fdfind to not shadow
# another 'fd' command
[[ -x $(which fdfind) ]] && alias fd="fdfind"

alias ru="repoutil unclean"
alias rs="repoutil stat"
alias rl="repoutil list"
alias rf="repoutil fetch"
alias rb="repoutil branchstat | sed -e 's/.*code\///' | sort | column -s'|' -t"

if [[ -x "$HOME/.cargo/bin/exa" ]]; then
    default_exa="exa --group-directories-first"
    alias ls="$default_exa"
    alias ll="$default_exa --long --git"
    alias la="$default_exa --long -a --git"
    alias lt="$default_exa --tree"
else
    alias ls="ls --color --group-directories-first"
    alias ll="ls -l"
    alias la="ls -l -a"
fi

alias dlaq="dla -q"
alias dls="cat ~/.download"

