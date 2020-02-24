function s:markdown_goto_file()
    try
        normal! gf
    catch
        try
            normal! vi("by
            execute "edit " . getreg("b")
        catch
            echo v:exception
        catch /^Vim.*E447/
            echo "COULDN'T GOTO FILE " . v:exception
        endtry
    endtry
endfunction
command! GotoFile call s:markdown_goto_file()

