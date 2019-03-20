if exists('g:typewriter_enabled')
    finish
endif
let g:typewriter_enabled=1

let g:typewriter_mode_active=0
function! s:ToggleTypewriting(bang)
    if a:bang != "!"
        nnoremap <silent> j @='jzz'<CR>
        nnoremap <silent> k @='kzz'<CR>
        vnoremap <silent> j @='jzz'<CR>
        vnoremap <silent> k @='kzz'<CR>
        let g:typewriter_mode_active=1
    else
        nnoremap <silent> j j
        nnoremap <silent> k k
        vnoremap <silent> j j
        vnoremap <silent> k k
        let g:typewriter_mode_active=0
    endif
endfunction

command! -bang Typewrite call <SID>ToggleTypewriting("<bang>")
