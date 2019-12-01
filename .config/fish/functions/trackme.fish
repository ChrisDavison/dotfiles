function trackme
    set title (string replace -a ' ' '-' "$argv")
    set fn (date +"$HOME/Dropbox/self-tracking/%Y%m%d-$title.txt")
    echo "title: $argv" > $fn
    echo "value: " > $fn
    echo "tags: " > $fn
	nvim $fn
end
