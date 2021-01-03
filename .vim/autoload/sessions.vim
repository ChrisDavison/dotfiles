function! sessions#save_last()
    exec "!rm ~/.lastsession.vim"
    mks ~/.lastsession.vim
endfunction

