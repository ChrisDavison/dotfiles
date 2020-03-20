function! s:maybe_filetype_markdown()
    if exists('g:forced_plaintext_files') && (index(g:forced_plaintext_files, expand('%')) >= 0)
        return
    elseif &filetype == "help" || expand('%:p') =~ "doc/"
        setlocal filetype=help
    else
        setlocal filetype=markdown.pandoc
    endif
endfunction

au BufNewFile,BufFilePre,BufRead *.txt,*.md call s:maybe_filetype_markdown()

