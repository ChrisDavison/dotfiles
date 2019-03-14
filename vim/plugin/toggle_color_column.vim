" Toggle color column {{{2
function! s:ToggleColorcolumn()
    if &colorcolumn > 0
        set colorcolumn=0
    else
        set colorcolumn=80
    endif
endfunction
command! ToggleColorColumn call s:ToggleColorcolumn()
" }}}2
