if &compatible || exists('g:loaded_thesisnotes')
    finish
endif
let g:loaded_thesisnotes = 1

function! s:ThesisNotes(vertical)
    if match(expand("%:p"), "thesis") >= 0
        if expand("%:p:h:t") == "notes"
            exec ":e ../" . expand("%:n") | normal `z
        else
			exec ":normal mz"
            if a:vertical == "!"
                exec ":vspl notes/" . expand("%:n")
            else
                exec ":spl notes/" . expand("%:n")
            endif
        endif
    endif
endfunction
command! -bang ThesisNotes exec <SID>ThesisNotes("<bang>")
