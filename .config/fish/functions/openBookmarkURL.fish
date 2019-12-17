function openBookmarkURL
	set bm $argv[1]
    set url (cat $bm | grep url | cut -d' ' -f2-)
    uname -a | rg -q Microsoft
    if test $status -eq 0
        '/mnt/c/Program Files/Firefox Developer Edition/firefox.exe' $url
    else
        xdg-open $url
    end
end
