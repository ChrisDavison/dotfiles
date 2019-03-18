if exists('g:loaded_striptrailingwhitespace')
    finish
endif
let g:loaded_striptrailingwhitespace = 1

function! StripTrailingWhitespace()
    if !&binary && &filetype != 'diff'
        if getpos("'z")[1] != getpos(".")[1]
            normal mz
        endif
        %s/\s\+$//e
        if getpos("'z")[2] != 0
            normal `z
            delm z
        endif
    endif
endfunction
