function newlit
    if test (count $argv) -eq 0
        set url (string join "-" (string split " " (read -P "url: ")))
        set filename (string join "-" (string split " " (read -P "filename: ")))
    else if test (count $argv) -eq 1
        set url $argv[1]
        echo "One arg passed. Assuming it's the URL."
        set filename (string join "-" (string split " " (read -P "filename: ")))
    else
        set url $argv[1]
        set filename (string join '-' $argv[2..(count $argv)])
    end
    set ext (string split -r -m1 . $url)[2]
    echo "# $filename" > $HOME/Dropbox/literature/writeups/$filename.txt
    echo >> $HOME/Dropbox/literature/writeups/$filename.txt
    echo >> $HOME/Dropbox/literature/writeups/$filename.txt
    echo "@unread" >> $HOME/Dropbox/literature/writeups/$filename.txt
    curl $url --output $HOME/Dropbox/literature/papers/$filename.$ext
end
