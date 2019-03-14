function! ThesisNotes()
    if match(expand("%:p"), "thesis") >= 0
        if expand("%:p:h:t") == "notes"
            exec ":e ../" . expand("%:n") | normal `z
        else
			exec ":normal mz"
            exec ":e notes/" . expand("%:n")
        endif
    endif
endfunction
command! ThesisNotes exec ThesisNotes()
