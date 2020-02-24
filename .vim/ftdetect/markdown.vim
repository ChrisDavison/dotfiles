function! s:maybe_filetype_markdown()
    if &filetype == "help" || expand('%:p') =~ "doc/"
        setlocal filetype=help
    else
        setlocal filetype=markdown.pandoc
    endif
endfunction

au BufEnter *.txt,*.md,.scratch call s:maybe_filetype_markdown()
