setlocal iskeyword=a-z,A-Z,_,48-57

command! RunPyInTmux exec 'SlimeSend1 %run ' . expand('%:t')
nnoremap <leader>r :RunPyInTmux<CR>
