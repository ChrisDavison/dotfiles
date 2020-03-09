#!/usr/bin/env sh
[ $# -lt 1 ] && echo "Usage: youtube (video|audio|tidyurl) url" && return 1
cmd=${1:-''}; shift
tidyurl() {
    no_time=$(echo "$1" | rg --passthru "&t=\d+s" -r '')
    no_playlist=$(echo "$no_time" | rg --passthru "&list=[a-zA-Z0-9_]+" -r '')
    no_playlist_index=$(echo $no_playlist | rg --passthru "&index=\d+" -r '')
    echo $no_playlist_index
}

case "$cmd" in
    video)
        tidied=$(tidyurl "$1")
        format="%(title)s-%(id)s-%(format_id)s.%(ext)s"
        youtube-dl -f bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best --merge-output-format mp4 -o "$format" "$tidied"
        ;;
    audio)
        tidied=$(tidyurl "$1")
        format="%(title)s-%(id)s-%(format_id)s.%(ext)s"
        youtube-dl --prefer-ffmpeg -f 171/251/140/bestaudio --extract-audio --audio-format mp3 --audio-quality 0 -o "$format" "$tidied"
        ;;
    tidyurl) tidyurl "$1" ;;
    *) echo "Usage: youtube (video|audio|tidyurl) url" ;;
esac

