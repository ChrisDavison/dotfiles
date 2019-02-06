cv() { # Choose a Vim session
    if [ -d ~/.vim-sessions ]; then
        selected=$(fd . ~/.vim-sessions | fzf -q "$1")
        [[ -n "$selected" ]] && $EDITOR -S "$selected"
    else
        echo "Couldn't find ~/.vim-sessions folder"
    fi
}

choose_tmux_session() {
    if tmux list-sessions 2>&1 > /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
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

asmrfind() {
    [ -z "$ASMRFILE" ] && echo "Need to define ASMRFILE" && return 1
    query=${@:-".*"}
    if [ "$1" = "+" ]; then
        shift
        query="^\+.*$@"
    fi
    cat -s "$ASMRFILE" | sort | rg "$query"
}

asmradd() {
    [ -z "$ASMRFILE" ] && echo "Need to define ASMRFILE" && return 1
    read -p "Author: " vid_author
    read -p "Title: " vid_title
    read -p "ID: " vid_hash
    echo "$vid_author: $vid_title;$vid_hash" >> "$ASMRFILE"
}

asmr() {
    match=$(asmrfind "$@" | random_line)
    [[ -z "${match}" ]] && echo "No match" && return 1
    echo "Watching: $(echo $match | cut -d';' -f1)"
    echo "https://youtube.com/watch?v=$(echo $match | cut -d';' -f2)" | OpenInBrowser
}


OpenInBrowser() {
    read url
    [ -z $url ] && url="$@"
    [ -z $url ] && echo "Empty url" && return 1
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
    if [[ ! -d "${NOTESDIR}" || ! -d "${NOTESBACKUPDIR}" ]]; then
        echo "NOTESDIR and NOTESBACKUPDIR must both be defined"
        echo "NOTESDIR: ${NOTESDIR}"
        echo "NOTESBACKUPDIR: ${NOTESBACKUPDIR}"
        return 1
    fi
    dt=$(date +"%Y%m%dT%H%M")
    echo "${dt}: $NOTESDIR Backup"
    rm -rf "${NOTESBACKUPDIR}/"*
    cp -r "${NOTESDIR}"/* "${NOTESBACKUPDIR}/"
    cd "${NOTESBACKUPDIR}"
    git add . > /dev/null
    git commit -m "Backup ${dt}"
    git push
    git archive -o $HOME/notes-backup--${dt}.zip @
}

nf() { # Find inside notes
    # If I'm inside NOTESDIR, only search the subdirectory
    [[ ! -d "${NOTESDIR}" ]] && echo "NOTESDIR not defined" &&  return 2
    loc="${NOTESDIR}"
    if [ -d "$1" ]; then
        loc="$1"
        shift
    fi
    [[ -z "$@" ]] && echo "Must pass a query" && return 1;
    echo "Match is (F)ilename, (D)irectory, or (C)ontent"
    fd "$@" "${loc}" -e md | sed -e "s/^/(F) /"
    fd "$@" "${loc}" -t d | sed -e "s/^/(D) /"
    rg "$@" "${loc}" -l | sed -e "s/^/(C) /"
}

nff() { # Find in note titles only
    nf "$@" | rg "^\(F\)" | cut -d' ' -f2-
}
nfc() { # Find in note contents only
    nf "$@" | rg "^\(C\)" | cut -d' ' -f2-
}

nffc() {
    code $(nff "$@" | fzf --multi)
}

nfcc() {
    code $(nfc "$@" | fzf --multi)
}

mdlinks() {
    [[ $PWD/ = "${NOTESDIR}"/* ]] && loc="." || loc="${NOTESDIR}"
    rg "[^!]\[.*?\]\(.*?\)" "$loc" -g "*.md" -o --no-heading --sort=path
}

mdimages() {
    [[ $PWD/ = "${NOTESDIR}"/* ]] && loc="." || loc="${NOTESDIR}"
    rg "!\[.*?\]\(.*?\)" "$loc" -g "*.md" -o --no-heading --sort=path
}

mdhashtags() {
    [[ $PWD/ = "${NOTESDIR}"/* ]] && loc="." || loc="${NOTESDIR}"
    rg "(?:[\s\`^])#[a-zA-Z]+" "$loc" -g "*.md" -o --no-heading --sort=path
}
