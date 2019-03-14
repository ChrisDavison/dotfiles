function! ToggleConceal()
    if &conceallevel == 2
        set conceallevel=0
    else
        set conceallevel=2
    endif
endfunction
