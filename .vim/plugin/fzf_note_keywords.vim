function! s:find_tag(tag)
    exec "Rg @" . a:tag
    normal i
endfunction

command! Tagsearch call fzf#run(fzf#wrap({
            \ 'source': 'tagsearch --long', 
            \ 'sink': function("<SID>find_tag")}))

