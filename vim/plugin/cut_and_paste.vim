" When in Normal mode, paste over the current line...
nmap  <C-P> 0d$"*p

" When in Visual mode, paste over the selected region...
xmap  <C-P> "*pgv

" In Normal mode, yank the entire buffer...
nmap <C-C> 1G"*yG``:call YankedToClipboard()<CR>

" In Visual mode, yank the selection...
xmap  <C-C> "*y:call YankedToClipboard()<CR>

function! YankedToClipboard ()
    let block_of = (visualmode() == "\<C-V>" ? 'block of ' : '')
    let N = strlen(substitute(@*, '[^\n]\|\n$', '', 'g')) + 1
    let lines = (N == 1 ? 'line' : 'lines')
    echo block_of . N lines 'yanked to clipboard'
endfunction
