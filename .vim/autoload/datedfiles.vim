function! datedfiles#new(root)
    let filename=expand(a:root . strftime("/%Y%m%d-%A.md"))
    if filereadable(l:filename)
        exec "e " . l:filename
        normal G
    else
        exec "e " . l:filename
        exec "norm i" . strftime("# %Y-%m-%d %A")
        norm o
        norm o
    endif
endfunction


