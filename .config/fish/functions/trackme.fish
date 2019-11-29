function trackme
    set title (string replace -a ' ' '-' "$argv")
    set fn (date +"%Y%m%d-$title.txt")
    echo "title: $argv" > $fn
	nvim $fn
end
