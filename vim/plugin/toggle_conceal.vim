if exists('g:loaded_toggleconceal')
    finish
endif
let g:loaded_toggleconceal=1

function! ToggleConceal()
    if &conceallevel == 2
        set conceallevel=0
    else
        set conceallevel=2
    endif
endfunction
