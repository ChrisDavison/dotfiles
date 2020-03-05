function! s:open_link_from_buffer(line)
    let matches=matchlist(a:line, "](\\(.*\\))")
    if len(matches) > 1
        let match = matches[1]
        if filereadable(l:match)
            exec "e " . l:match
        else
            exec "!firefox " . l:match
        end
    endif
endfunction

command! -bang LinksInBuffer call fzf#run(fzf#wrap({
            \ 'source': 'rg --only-matching "\[.*\]\(.*\)" ' . expand("%"),
            \ 'sink': function("<SID>open_link_from_buffer"),
            \ 'options': '--exact --prompt Link: '}))

