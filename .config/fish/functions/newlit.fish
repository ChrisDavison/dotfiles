function newlit
	set url $argv[1]
    set target (string join '-' $argv[2..(count $argv)])
    echo $url
    set stem (string split -r -m1 . $target)[1]
    echo "# $stem" > writeups/$stem.txt
    echo >> writeups/$stem.txt
    echo >> writeups/$stem.txt
    echo "@unread" >> writeups/$stem.txt
    curl $url --output papers/$target.pdf
end
