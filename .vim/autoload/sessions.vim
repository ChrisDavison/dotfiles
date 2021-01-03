function! sessions#save_last()
    exec "!rm ~/.lastsession.vim"
    mks ~/.lastsession.vim
endfunction
command! SaveLastSession call sessions#save_last()
