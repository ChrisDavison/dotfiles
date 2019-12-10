function newlit
	set url $argv[1]
set target $argv[2]
echo $url
set stem (string split -r -m1 . $target)[1]
echo "# $stem" > writeups/$stem.txt
echo >> writeups/$stem.txt
echo >> writeups/$stem.txt
echo "@unread" >> writeups/$stem.txt
curl $url --output papers/$target
end
