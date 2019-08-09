set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case

command! -bang -nargs=* Rg
            \ call fzf#vim#grep(
            \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1, 
            \ fzf#vim#with_preview('right:50%:hidden', '?'),
            \ <bang>0)

" CTRL-A CTRL-Q to select all and build quickfix list

function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
