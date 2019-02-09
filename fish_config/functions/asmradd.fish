function asmradd
    if [ -z "$ASMRFILE" ]
        echo "Need to define ASMRFILE"
        return 1
    end
    read -P "Author: " vid_author
    read -P "Title: " vid_title
    read -P "ID: " vid_hash
    echo "$vid_author: $vid_title;$vid_hash" >> "$ASMRFILE"
end
