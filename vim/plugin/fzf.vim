set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case

command! -bang -nargs=* Rg
            \ call fzf#vim#grep(
            \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1, 
            \ fzf#vim#with_preview('right:50%:hidden', '?'),
            \ <bang>0)
