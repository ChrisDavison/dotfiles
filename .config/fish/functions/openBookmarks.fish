function openBookmarks
    if count $argv > 1
        pushd ~/Dropbox/bookmarks
        for bm in (tagsearch $argv | fzf -m)
            openBookmarkURL $bm
        end
        popd
    else
        echo "Must pass some keywords. Too many bookmarks"
    end
end
