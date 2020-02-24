function s:markdown_goto_file()
    let fname=expand("<cfile>")
    if filereadable(l:fname)
        execute "edit " . l:fname
    else
        if getline(".")[col(".")-1:col(".")] == 'y'
            normal vi("by
        else
            normal f]
            normal vi("by
            execute "edit " . getreg("b")
        end
    end
endfunction
command! GotoFile call s:markdown_goto_file()

