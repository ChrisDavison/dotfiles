if &compatible || exists('g:loaded_thesisnotes')
    finish
endif
let g:loaded_thesisnotes = 1

function! s:ThesisNotes()
    if match(expand("%:p"), "thesis") >= 0
        if expand("%:p:h:t") == "notes"
            exec ":e ../" . expand("%:n") | normal `z
        else
			exec ":normal mz"
            exec ":e notes/" . expand("%:n")
        endif
    endif
endfunction
command! ThesisNotes exec <SID>ThesisNotes()
