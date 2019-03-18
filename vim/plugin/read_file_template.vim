function! ReadFileTemplate()
    let ext = expand("%:e")
    let dir = globpath(&rtp, "file_templates")
    if isdirectory(dir)
        let fname = dir . "/template." . ext
        if filereadable(fname)
            exec "read " . fname
            normal ggdd
        endif
    else
        echom "ReadFileTemplate: Couldn't find template dir"
        return
    endif
endfunction

function! s:ReadTemplate(fname)
    normal mA
    let dir = globpath(&rtp, "file_templates")
    let fname = dir . '/' . a:fname
    if filereadable(fname)
        normal dd
        exec "read " . fname
    else
        echom "ReadTemplate: Couldn't find file"
        normal `AmA
        return
    endif
endfunction

command! -nargs=1 ReadTemplate call <SID>ReadTemplate(<f-args>)
