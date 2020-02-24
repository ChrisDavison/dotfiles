function s:markdown_goto_file()
    let fname=expand("<cfile>")
    if filereadable(l:fname)
        execute "edit " . l:fname
    else
        try
            normal! vi("by
            execute "edit " . getreg("b")
        catch
            echo v:exception
        catch /^Vim.*E447/
            echo "COULDN'T GOTO FILE " . v:exception
        endtry
    end
endfunction
command! GotoFile call s:markdown_goto_file()

