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

inpath() { 
    type "$1" >/dev/null 2>&1; 
}

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
    rsync -az  "${NOTESDIR}"/* "${NOTESBACKUPDIR}/"
    pushd "${NOTESBACKUPDIR}"
    git add . > /dev/null
    git commit -m "Backup ${dt}"
    git push
    git archive -o $HOME/notes-backup--${dt}.zip @
    popd
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
    echo "Match is Filename, Directory, or Content"
    fd "$@" "${loc}" -e md | sed -e "s/^/F,/"
    fd "$@" "${loc}" -t d | sed -e "s/^/D,/"
    rg "$@" "${loc}" -l | sed -e "s/^/C,/"
}

mdstructure(){
    cmd="$1"; shift
    case "$cmd" in
        links) query="[^!]\[.*?\]\(.*?\)" ;;
        images) query="!\[.*?\]\(.*?\)" ;;
        keywords) query="(?:[\s\`^])#[a-zA-Z]+" ;;
        headers) query="^#+ .*" ;;
        *) echo "Unrecognised command: $cmd" && return 1 ;;
    esac
    rg "$query" "$@" -g -o --no-heading --sort=path
}

todobackup() {
    cp "${NOTESDIR}/todo.md" "${NOTESBACKUPDIR}/todo.md"
    cp "${NOTESDIR}/done.md" "${NOTESBACKUPDIR}/done.md"
    pushd ${NOTESBACKUPDIR}
    date=$(date +"%Y-%m-%dT%H%M%S")
    echo "TODO backup ${date}"
    git add .
    git commit -m "TODO backup ${date}"
    git push
    popd
}

noext() {
    echo "${1%.*}"
}

sanitise() {
    direc=$(dirname $1)
    base=$(basename $1)
    echo $base | tr '[:upper:]' '[:lower:]' | sed 's/[^a-zA-Z0-9.-]/-/g' | tr -s - - | sed 's/\-$//g'
}

ppath() {
    echo "$path" | tr ':' '\n'
}

fromepoch() {
    date -r "$1" +"%Y%m%d %H:%M:%S"
}

# Get audio or video from youtube, or tidy a youtube url
youtube() {
    cmd="$1"; shift
    case "$cmd" in
        video) 
            tidied=$(_tidy_youtube_url "$1")
            format="%(title)s-%(id)s-%(format_id)s.%(ext)s"
            youtube-dl -f bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best --merge-output-format mp4 -o "$format" "$tidied"
            ;;
        audio) 
            tidied=$(_tidy_youtube_url "$1")
            format="%(title)s-%(id)s-%(format_id)s.%(ext)s"
            youtube-dl --prefer-ffmpeg -f 171/251/140/bestaudio --extract-audio --audio-format mp3 --audio-quality 0 -o "$format" "$tidied"
            ;;
        tidyurl)
            echo "$1" | rg "&t=\d+s" -r '' | rg "&list=[a-zA-Z0-9_]+" -r '' | rg "&index=\d+" -r ''
            ;;
    esac
}

# Highlight either a date (d=digit: dddd-dd-dd), or a keyword (+WORD)
t() {
    ~/.cargo/bin/t "$@" | rg --passthru "\+\w|\b\d\d\d\d-\d\d-\d\d\b"
}

financeadd(){
    if [ -z "$FINANCEFILE" ]; then
        echo "Need to define FINANCEFILE"
        return 1
    fi
    printf "Date: " && read ddate
    printf "Cost: " && read cost
    printf "Description: " && read desc
    printf "Category: " && read category
    echo $ddate","$cost","$desc","$category >> $FINANCEFILE
}

