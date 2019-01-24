# Functions
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

# FZF
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
        if [[ -d "$selected" ]]; then
            cd $selected
        else
            echo "Dir doesn't exist."
        fi
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

peek() {
    if inpath bat; then
        tmux split-window -p 33 bat "$@" || exit;
    else
        tmux split-window -p 33 "$EDITOR" "$@" || exit;
    fi
}

inpath() { type "$1" >/dev/null 2>&1; }

pager() {
    # Use PAGER, defaulting to less, if outputting to a terminal
    if inpath bat; then
        bat "$@"
    elif [ -t 1 ]; then
        ${PAGER:-less} "$@"
    else
        cat "$@"
    fi
}

page() {
    exec "$@" | pager
}

swap() {
    set -e
    mv "$2" "$1.$$"
    mv "$1" "$2"
    mv "$1.$$" "$1"
}


asmr() {
    ASMRFILE=~/Dropbox/asmr.csv
    case "$1" in
        add|a)
            read -p "Author: " vid_author
            read -p "Title: " vid_title
            read -p "ID: " vid_hash
            echo "$vid_author: $vid_title;$vid_hash" >> "$ASMRFILE"  ;;
        find|filter)
            query=$(echo "$@" | sed "s/ /|/g")
            cat "$ASMRFILE" | sort | rg "$query" | cut -d';' -f1 | column -s':' -t ;;
        findfav)
            query=$(echo "$@" | sed "s/ /|/g")
            cat "$ASMRFILE" | sort | rg "^\*$query" | cut -d';' -f1 | column -s':' -t ;;
        vids|list)
            cat -s "$ASMRFILE" | sort | cut -d';' -f1 | column -s':' -t ;;
        authors|artists)
            cat -s "$ASMRFILE" | cut -d'-' -f1 | sort | uniq ;;
        favs|top10)
            cat -s "$ASMRFILE" | rg "^\*" | cut -d';' -f1 | sort | uniq ;;
        fav)
            shift
            query=$(echo "$@" | sed "s/ /|/g")
            if [ $# -gt 1 ]; then
                query="($query)"
            fi
            query="^\*.*$query"
            if inpath shuf; then
                match=$(cat "$ASMRFILE" | rg "$query" | shuf -n1)
            elif inpath gshuf; then
                match=$(cat "$ASMRFILE" | rg "$query" | gshuf -n1)
            else
                echo "Couldn't find shuf or gshuf to choose random vid"
                return 1
            fi
            if [ "$match" != "" ]; then
                echo "Playing '$(cut -d";" -f1 <(echo $match))'"
                url="https://youtube.com/watch?v="$(cut -d";" -f2 < <(echo $match))
                OpenInBrowser ${url}
            else
                echo "No favourite videos"
            fi
            ;;
        *)
            # Make query an "OR" regex pattern
            query=$(echo "$@" | sed "s/ /|/g")
            # Find all lines matching, and select only 1 from a random shuffle
            # Pipe cat to rg, so that I can switch to an external curl request
            # if I wish to avoid the local file.
            if inpath shuf; then
                match=$(cat "$ASMRFILE" | rg "${query}" | shuf -n1)
            elif inpath gshuf; then
                match=$(cat "$ASMRFILE" | rg "${query}" | gshuf -n1)
            else
                echo "Couldn't find shuf or gshuf to choose random vid"
                return 1
            fi
            url=""
            if [[ -z ${match} ]]; then
                # If we've NOT got a match, build a youtube search url
                echo "No match. Searching for ${query}"
                joined=$(echo "${query}" | sed "s/|/+/g")
                url="https://www.youtube.com/results?search_query=asmr+${joined}"
            else
                # Otherwise, just set the url to that of the ASMR video
                echo "Playing '$(cut -d";" -f1 <(echo ${match}))'"
                url="https://youtube.com/watch?v="$(cut -d";" -f2 < <(echo ${match}))
            fi
            OpenInBrowser ${url} ;;
    esac
}


OpenInBrowser() {
    url="$1"
    if inpath open; then
        open ${url}
    elif inpath firefox; then
        firefox ${url}
    elif inpath chrome; then
        chrome ${url}
    else
        echo "No browser..."
        return 2
    fi
}

notebackup() {
    dt=$(date +"%Y%m%dT%H%M")
    echo "Backup $dt"
    if [ -d "${NOTESDIR}" ]; then
        echo "Backing up $NOTESDIR"
        if [ -d "${NOTESBACKUPDIR}" ]; then
            rm -rf "${NOTESBACKUPDIR}/"*
            cp -r "${NOTESDIR}"/* "${NOTESBACKUPDIR}/"
            cd "${NOTESBACKUPDIR}"
            git add . > /dev/null
            git commit -m "Backup $dt"
            git push
            git archive -o $HOME/notes-backup--$(date +"%Y%m%dT%H%M").zip @
        else
            echo "NOTESBACKUPDIR not defined.  Must create env var and git repo."
            return -1
        fi
    else
        echo "NOTESDIR not defined.  Must create env var."
    fi
}

nf() {
    if [ -z "$@" ]; then
        echo "Must pass a query"
        return 1;
    fi
    if [ ! -d "${NOTESDIR}" ]; then
        echo "NOTESDIR not defined"
        return 2;
    fi
    echo "Matching filename"
    echo "================="
    find "${NOTESDIR}" -type f | rg "$@"

    echo
    echo "Matching contents"
    echo "================="
    rg "$@" "${NOTESDIR}" -l
}

mdsearch() {
    _usage="usage:
    link                   markdown url links
    img|image|picture      markdown image links
    hashtag|tag|keyword    hashtags (maybe inaccurate)"
    loc="${NOTESDIR}"
    if [[ $PWD/ = "${NOTESDIR}"/* ]]; then
        loc="."
    fi
    case $1 in
        link)
            rg "[^!]\[.*?\]\(.*?\)" "$loc" -g "*.md" -o --no-heading --sort=path ;;
        img|image|picture)
            rg "!\[.*?\]\(.*?\)" "$loc" -g "*.md" -o --no-heading --sort=path ;;
        hashtag|tag|keyword)
            rg "(?:[\s\`^])#[a-zA-Z]+" "$loc" -g "*.md" -o --no-heading --sort=path ;;
        *) echo "$_usage" ;;
    esac
}
