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

peek() { # Either bat or EIDOT a file
    if inpath bat; then
        tmux split-window -p 33 bat "$@" || exit;
    else
        tmux split-window -p 33 "$EDITOR" "$@" || exit;
    fi
}

inpath() { # Check ifa file is in $PATH
    type "$1" >/dev/null 2>&1;
}

pager() { # Use PAGER, defaulting to less
    if inpath bat; then
        bat "$@"
    elif [ -t 1 ]; then
        ${PAGER:-less} "$@"
    else
        cat "$@"
    fi
}

page() { # Execute a path and pipe to PAGER
    exec "$@" | pager
}

swap() { # Swap two files (move, using a temporary)
    set -e
    mv "$2" "$1.$$"
    mv "$1" "$2"
    mv "$1.$$" "$1"
}

OpenInBrowser() { # Open link in whichever browser is in path
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

note(){ # Use vim to open files selected with `notes`
    files=$(notes)
    [ -z "$files" ] && return 1
    v "$files"
}

notes(){ # Use fzf and bat to preview, and select, notes
    query=${1:-''}
    batcmd='bat $NOTESDIR/{} --color=always -n'
    fd . -e md "$NOTESDIR" | sed -e "s!$NOTESDIR/!!g" | fzf -q "$query" --multi --preview="$batcmd" --preview-window=down:50% | sed -e "s!^!$NOTESDIR/!g"
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
    fd "$@" "${loc}" -e md | sed -e "s/^/F /"
    fd "$@" "${loc}" -t d | sed -e "s/^/D /"
    rg -F "$@" "${loc}" -l | sed -e "s/^/C /"
}

mdstructure(){ # Display links, images, keywords, or headers for MD files
    cmd="$1"; shift
    case "$cmd" in
        links) query="[^!]\[.*?\]\(.*?\)" ;;
        images) query="!\[.*?\]\(.*?\)" ;;
        keywords) query="(?:[\s\`^])#[a-zA-Z]+" ;;
        headers) query="^#+ .*" ;;
        todos) query="^\s*\-*\s*\[ \]\s*.*" ;;
        *)
            echo "Unrecognised command: $cmd"
            echo "links, images, keywords, todos, or headers"
            return 1
            ;;
    esac
    if [ $# -gt 0 ]; then
        files="$@"
    else
        files=`echo **/*.md`
    fi
    rg "$query" $files -g -o --no-heading --sort=path
}

noext() { # Remove extension from file
    echo "${1%.*}"
}

sanitise() { # Tidy up a filename
    direc=$(dirname $1)
    base=$(basename $1)
    echo $base | tr '[:upper:]' '[:lower:]' | sed 's/[^a-zA-Z0-9.-]/-/g' | tr -s - - | sed 's/\-$//g'
}

ppath() { # Pretty print $PATH
    echo "$path" | tr ':' '\n'
}

fromepoch() { # Convert from epoch seconds to YYYYmmdd HHMMSS
    date -r "$1" +"%Y%m%d %H:%M:%S"
}

youtube() { # Get audio, video, or tidyurl from youtube
    [ $# -lt 1 ] && echo "Usage: youtube (video|audio|tidyurl) url" && return 1
    cmd=${1:-''}; shift
    case "$cmd" in
        video)
            tidied=$(youtube tidyurl "$1")
            format="%(title)s-%(id)s-%(format_id)s.%(ext)s"
            youtube-dl -f bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best --merge-output-format mp4 -o "$format" "$tidied"
            ;;
        audio)
            tidied=$(youtube tidyurl "$1")
            format="%(title)s-%(id)s-%(format_id)s.%(ext)s"
            youtube-dl --prefer-ffmpeg -f 171/251/140/bestaudio --extract-audio --audio-format mp3 --audio-quality 0 -o "$format" "$tidied"
            ;;
        tidyurl)
            echo "$1" | rg "&t=\d+s" -r '' | rg "&list=[a-zA-Z0-9_]+" -r '' | rg "&index=\d+" -r ''
            ;;
        *) echo "Usage: youtube (video|audio|tidyurl) url" ;;
    esac
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

listfuncs() { # List functions in this file
    cat $SHELLFUNCS | rg "\w+\(\)" | column -s'{' -t
}

fh() { # Use FZF to preview history
    cat ~/.zsh_history | fzf -q "${1:-}" --preview='echo {} | bat' --preview-window=down:50%
}

mdtohtml() { # Convert a markdown file to html
    pandoc "$1" -o $(noext "$1").html --from markdown-simple_tables+subscript+superscript --filter pandoc-tablenos -s --toc --toc-depth=2 -c ~/src/github.com/chrisdavison/dotfiles/simple.css -s --mathjax
}

linkedtobin(){ # View all entires in ~/bin that are symlinks to my scripts
    ls -l ~/bin | awk -F' ' '/-> .*scripts.*/{print $7":"$9}' | column -s':' -t
}


dashboardflask(){
    FLASK_APP=server.py FLASK_DEBUG=True flask run
}

dbxarchive(){
    archivedir="$HOME/Dropbox/archive"
    if [ -d "$archivedir" ]; then
        echo "Moving $@ to Dropbox archive"
        mv $@ "$archivedir"
    else
        echo "Couldn't find Dropbox archive directory $archivedir"
    fi
}

add2md(){
    dest=$1
    dest_base=$(dirname $dest)
    file_dir="assets"
    target=$dest_base/$file_dir
    [ ! -f "$dest" ] && echo "No note file: $dest" && return 1
    [ ! -d "$target" ] && echo "No dir: $target" && return 2
    shift
    echo "Linking notes to $dest"
    for fn in $@
    do
        fn_short=$(basename $fn)
        echo "Move $fn_short to $target"
        echo "- [$fn_short](./$file_dir/$fn_short)" >> $dest
    done
    echo "===== TAIL OF THE NOTE FILE ====="
    tail -n $(( $# + 2 )) $dest
}

todobackup() { # Backup only todo files to notes repo
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


todo(){
    echo "-   [ ] $@" >> "$HOME/Dropbox/inbox.md"
}

todos(){
    rg "^\s*\-\s+\[ \]\s+(.*)$" -r '$1' "$HOME/Dropbox/inbox.md" --color=never | column -s':' -t
}

ta(){ # Add a todo
    echo "- $@" >> $TODOFILE
}

# Add a note
n(){
    notesfile="$HOME/Dropbox/notes/notes.txt"
    if [ ! -f "$notesfile" ]; then
        touch $notesfile
    fi
    echo "- $@" >> $notesfile
}

# View notes
nn(){
    bat -l md "$HOME/Dropbox/notes/notes.txt"
}

# Clear out notes
nnd() {
    rm "$HOME/Dropbox/notes/notes.txt"
    touch "$HOME/Dropbox/notes/notes.txt"
}
