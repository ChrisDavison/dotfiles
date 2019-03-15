" Toggle color column
function! s:ToggleColorcolumn()
    if &colorcolumn > 0
        set colorcolumn=0
    else
        set colorcolumn=80
    endif
endfunction
command! ToggleColorColumn call s:ToggleColorcolumn()
