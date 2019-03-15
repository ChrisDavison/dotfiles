" Create directory if it doesn't exist, on write
function! MakeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction

augroup nonExDir
    autocmd!
    autocmd BufWritePre * call MakeNonExDir()   " Use my rtp func
augroup END
