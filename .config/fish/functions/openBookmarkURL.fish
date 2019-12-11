function openBookmarkURL
	set bm $argv[1]
    set url (cat $bm | grep url | cut -d' ' -f2-)
    xdg-open $url
end
