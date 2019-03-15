function! ReadFileTemplate()
    let ext = expand("%:e")
    let fname = g:file_template_dir . "template." . ext
    if filereadable(fname)
        exec "read " . fname
        normal ggdd
    endif
endfunction
