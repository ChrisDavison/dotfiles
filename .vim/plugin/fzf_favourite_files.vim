function! s:edit_favourite(key)
    let names=map(copy(g:fzf_favourite_files), {_, v -> v["name"]})
    let paths=map(copy(g:fzf_favourite_files), {_, v -> v["path"]})
    let idx=index(names, a:key)
    if idx == -1
        return
    end
    let filename = expand(paths[idx])
    if isdirectory(l:filename)
        exec "Explore " . l:filename
    elseif filereadable(l:filename)
        exec "e " . l:filename
    else
        echom "Can't open file/dir " . l:filename
    endif
endfunction

command! Favourites call fzf#run(fzf#wrap({
            \ 'source': reverse(map(copy(g:fzf_favourite_files), {_, v -> v["name"]})), 
            \ 'sink': function("<SID>edit_favourite")}))

