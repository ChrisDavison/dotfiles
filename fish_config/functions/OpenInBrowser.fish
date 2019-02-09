function OpenInBrowser
    set url "$argv"
    if [ -z "$url" ]
        read -P "URL: " url
    end
    if [ -z $url ]
        echo "Empty URL"
        return 1
    end
    if type -q open
        open $url
    else if type -q firefox
        firefox $url
    else if type -q chrome
        chrome $url
    else
        echo "No browser..."
        return 2
    end
end
