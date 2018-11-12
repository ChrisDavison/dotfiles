#     $$$$$$$$\
#     $$  _____|
#     $$ |   $$\   $$\ $$$$$$$\   $$$$$$$\
#     $$$$$\ $$ |  $$ |$$  __$$\ $$  _____|
#     $$  __|$$ |  $$ |$$ |  $$ |$$ /
#     $$ |   $$ |  $$ |$$ |  $$ |$$ |
#     $$ |   \$$$$$$  |$$ |  $$ |\$$$$$$$\
#     \__|    \______/ \__|  \__| \_______|

cv() { # Choose a Vim session
    if [ -d ~/.vim-sessions ]; then
        selected=$(fd . ~/.vim-sessions | fzf -q "$1")
        [[ -n "$selected" ]] && $EDITOR -S "$selected"
    else
        echo "Couldn't find ~/.vim-sessions folder"
    fi
}

venv() { # Choose a python env in ~/.envs
    if [ -d ~/.envs ]; then
        selected=$(fd "\bactivate$" ~/.envs | fzf -1 -q "$1")
        [[ -n "$selected" ]] && source "$selected"
    else
        echo "Couldn't find ~/.envs folder"
    fi
}

envv() { # Activate a virtual env from the root of this git repo
    find $(git rev-parse --show-toplevel) -regex ".*activate$"
}

choose_tmux_session() {
    if tmux list-sessions 2>&1 > /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
}

mcd() { # Make a directory, then switch to it
    # Make, and switch to, a directory
    if [ ! -n "$1" ]; then
        echo "Must pass directory as argument"
        return
    fi
    mkdir -p "$1"
    cd "$1"
}

_for_each_repo() {
    pushd "$HOME" >> /dev/null
    for repo in $HOME/devel/*; do
        if [ ! -d "$repo" ]; then
            continue
        fi
        cd "$repo"
        echo "Running $@ on $repo"
        $@
        echo
    done
    popd
}

copy_to_bin() { # Copy a file to ~/bin, without file extension
    cp "$1" ~/bin/$(noext $1)
}

link_to_bin() {
    ln -s "$1" ~/bin/$(noext $1)
}

newgit() { # Create a new project (touching readme and init commit)
    dir="$1"
    mkdir "$dir"
    cd "$dir"
    git init
    echo "# $dir" >> README.md
    git add README.md
    git commit -m "Initial commit"
    git status
}

capture(){ # Add a dated entry to file $CAPTUREFILE
    args="$@"
    d=$(date +"%F %T")
    if [ -f "$CAPTUREFILE" ]; then
        echo "- $d $args" >> "$CAPTUREFILE"
    else
        echo "Capturefile not defined"
    fi
}

list_dotfile_functions() {
    rg ".*\(\).*\{" ~/devel/dotfiles
}


#     $$$$$$$$\ $$$$$$$$\ $$$$$$$$\
#     $$  _____|\____$$  |$$  _____|
#     $$ |          $$  / $$ |
#     $$$$$\       $$  /  $$$$$\
#     $$  __|     $$  /   $$  __|
#     $$ |       $$  /    $$ |
#     $$ |      $$$$$$$$\ $$ |
#     \__|      \________|\__|
fbr() { # FZF through git branches
    local branches branch
    branches=$(git branch -vv)
    branch=$(echo "$branches" | fzf +m)
    if [[ -n "$branch" ]]; then
        git checkout $(echo "$branch" | awk {'print $1}' | sed "s/.* //")
    else
        echo "No branch given"
    fi
}

fco() { # FZF through branches and tags
    local tags branches target
    tags=$( git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}' ) || return
    branches=$(
      git branch --all | grep -v HEAD             |
      sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
      sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
    target=$(
      (echo "$tags"; echo "$branches") |
      fzf-tmux -- --ansi +m -d "\t" -n 2) || return
    git checkout $(echo "$target" | awk '{print $2}')
 }

fcoc() { # FZF through git commits
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

fshow(){ # FZF and show a commit
    git show $(git log --pretty=oneline | fzf -q "$1" | cut -d=' ' -f1)
}

vg() { # FZF through grep results, and open (multiple, using tab) files in vim
  local file

  file="$(rg --no-heading $@ | fzf -0 -1 -m | awk -F: '{print $1}')"

  if [[ -n $file ]]
  then
     vim $file
  fi
}

bm(){ # Jump to 'bookmark' directories (dirs in ~/.bm)
    local selected
    selected=$(cat ~/.bm | sed "s/#.*//g" | sed '/^\s*$/d' | fzf -1 -q "$1")
    if [[ -n $selected ]]; then
        cd $selected
    fi
}

add_bm(){ # Add current directory to ~/.bm (bookmarks)
    echo $(pwd) >> ~/.bm
    cat ~/.bm | sort | uniq > ~/.bm.bak
    mv ~/.bm.bak ~/.bm
}

fop(){ # Fuzzy-open file(s) (multiple, using `<TAB>`)
    open $(fzf -m -q "$1")
}


fza(){ # Fuzzy-add files to git (multiple, using `<TAB>`)
    selected=$(git ls-files -m -o --exclude-standard | fzf --print0 -m)
    [ -n ${selected} ] && git add ${selected}
}
