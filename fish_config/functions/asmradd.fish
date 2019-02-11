function asmradd
    if [ -z "$ASMRFILE" ]
        echo "Need to define ASMRFILE"
        return 1
    end
    read -P "Author: " vid_author
    read -P "Title: " vid_title
    read -P "Vid URL: " vid_url
    set vid_hash (youtubevidid $vid_url)
    # read -P "ID: " vid_hash
    if [ ! set -q vid_hash ]
        echo "Couldn't save video (vidhash empty)"
        return 1
    end
    echo "$vid_author: $vid_title;$vid_hash" >> "$ASMRFILE"
end
