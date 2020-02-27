function! s:find_tag(tag)
    exec "silent!Rg @" . a:tag
    call feedkeys("i")
endfunction

function! s:find_tag_grep(tag)
    exec "silent!grep @" . a:tag
endfunction

command! -bang Tagsearch call fzf#run(fzf#wrap({
            \ 'source': 'tagsearch --long', 
            \ 'sink': <bang>0 ? function("<SID>find_tag") : function("<SID>find_tag_grep")}))

