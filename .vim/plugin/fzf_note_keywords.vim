function! s:find_tag(tag)
    exec "silent!Rg @" . a:tag
    call feedkeys("i")
endfunction

command! Tagsearch call fzf#run(fzf#wrap({
            \ 'source': 'tagsearch --long', 
            \ 'sink': function("<SID>find_tag")}))

