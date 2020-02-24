function s:markdown_backlinks()
    call fzf#vim#grep(
                \ "rg --column --line-number --no-heading --color=always --smart-case -g '!tags' ".expand('%'), 1,
                \ fzf#vim#with_preview('right:50%:hidden', '?'), 0)
endfunction
command! Backlinks call s:markdown_backlinks()

