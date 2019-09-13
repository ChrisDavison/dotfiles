choose_tmux_session() {
    if tmux list-sessions 2>&1 > /dev/null ; then
        selected=$(tmux list-sessions | fzf -q "$1" | cut -d: -f1)
        [[ -n "$selected" ]] && tmux attach -d -t "$selected"
    else
        echo "No tmux sessions running."
    fi
}
alias tma=choose_tmux_session

inpath() { # Check ifa file is in $PATH
    type "$1" >/dev/null 2>&1;
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

noext() { # Remove extension from file
    echo "${1%.*}"
}

ppath() { # Pretty print $PATH
    echo "$PATH" | tr ':' '\n'
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

mdtohtml() { # Convert a markdown file to html
    pandoc "$1" -o $(noext "$1").html --from markdown-simple_tables+subscript+superscript --filter pandoc-tablenos -s --toc --toc-depth=2 -c ~/src/github.com/chrisdavison/dotfiles/simple.css -s --mathjax
}

linkedtobin(){ # View all entires in ~/bin that are symlinks to my scripts
    ls -l ~/bin | awk -F' ' '/-> .*scripts.*/{print $7":"$9}' | column -s':' -t
}

add2md(){ # Add an asset to an md file as a link
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

logbook() { # Open todays logbook in $EDITOR
    $EDITOR $(date +%"$HOME/Dropbox/notes/logbook/%Y/%Y-%m-%d.md")
}

logbooks(){
    a=${1:-1}
    b=${2:-1}
    fd . ~/Dropbox/notes/logbook -e md | sort -r | sed -n "$a","$b"p
}

logbook_recent() { # Display the last N logbooks (or from $1 to $2)
    a=${1:-1}
    b=${2:-10}
    bat `logbooks $a $b` --style=header,grid
}
alias lbr="logbook_recent"

logbook_search() { # Display logbooks with contents matching query
    bat $(rg "$@" ~/Dropbox/notes/logbook -l | sort -r) --style=header,grid
}
alias lbs="logbook_search"

shfuncs() { # List shell functions, functions in bashrc
    rg "^[a-zA-Z_]+\(" ~/.bashrc | column -t -s '{'
}

nonascii() { # Ripgrep for non-ascii, greek, or "£"
    rg "[^\x00-\x7F£\p{Greek}]" -o --no-heading
}

hex2dec() { # Convert passed hex values to decimal
    for f in $@; do
        printf "$f is %d\n" "$f"
    done
}

function cdl () {
    builtin cd "$1"
    ls
}

refresh_dmenu() {
    [ -f ~/.cache/dmenu_run ] && rm ~/.cache/dmenu_run && dmenu_path
}
