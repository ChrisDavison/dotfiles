function! s:edit_favourite(key)
    let filename = expand(g:fzf_favourite_files[a:key])
    if isdirectory(l:filename)
        exec "Explore " . l:filename
    elseif filereadable(l:filename)
        exec "e " . l:filename
    else
        echom "Can't open file/dir " . l:filename
    endif
endfunction

command! Favourites call fzf#run(fzf#wrap({
            \ 'source': reverse(sort(keys(g:fzf_favourite_files))), 
            \ 'sink': function("<SID>edit_favourite")}))

