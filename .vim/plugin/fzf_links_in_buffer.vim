function! s:open_link_from_buffer(line)
    echom len(matches)
endfunction

command! -bang LinksInBuffer call fzf#run(fzf#wrap({
            \ 'source': 'rg --only-matching "\[.*\]\(.*\)" ' . expand("%"),
            \ 'sink': function("<SID>open_link_from_buffer")}))

