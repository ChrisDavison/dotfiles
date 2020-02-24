let s:headermap={
            \'rust': 'fn',
            \'python': 'def',
            \'go': 'func',
            \'vim': 'function',
            \'markdown': '#\+'}
function s:goto_header(filter)
    let filt = len(a:filter) > 0 ? a:filter : ""
    let pattern="^\\s*" . s:headermap[&filetype] . "\\s*" .  l:filt
    exec "BLines" . pattern
endfunction
command! -nargs=* Headers call s:goto_header(<q-args>)
nnoremap <leader>i :<C-U>Headers<CR>
