function! s:search_mode_start()
    cnoremap <tab> <c-f>:resize 1<CR>a<c-n>
    let s:old_complete_opt = &completeopt
    let s:old_last_status = &laststatus
    set completeopt-=noinsert
    set laststatus=0
endfunction

function! s:search_mode_stop()
    try
        silent cunmap <tab>
    catch
    finally
        let &completeopt = s:old_complete_opt
        let &laststatus  = s:old_last_status
    endtry
endfunction

augroup SearchCompletions
    autocmd!
    autocmd CmdlineEnter [/\?] call <SID>search_mode_start()
    autocmd CmdlineLeave [/\?] call <SID>search_mode_stop()
augroup END
