augroup filetype_c
    autocmd!
    autocmd FileType c       set foldmethod=syntax
    autocmd FileType cpo     set foldmethod=syntax
    autocmd FileType arduino set foldmethod=syntax
augroup END

augroup filetype_py
    autocmd!
    autocmd FileType python  set foldmethod=indent
    autocmd FileType python  set tabstop=4
    autocmd FileType python  set softtabstop=4
augroup END

augroup filetype_json
    autocmd!
    autocmd FileType json    set tabstop=2
    autocmd FileType json    set softtabstop=2
    autocmd FileType json    set shiftwidth=2
augroup END

augroup filetype_go
    autocmd!
    autocmd FileType go      set nofen
    autocmd FileType go      set foldmethod=syntax
augroup END

augroup filetype_miscellany
    autocmd!
    autocmd BufNewFile,BufReadPost *.tex set filetype=tex
    autocmd FileType make    set noexpandtab
    autocmd FileType rust    set foldmethod=syntax
    autocmd FileType vim     set foldmethod=marker
    autocmd BufNewFile,BufReadPost *.es6 set filetype=javascript
augroup END

let b:javascript_fold=1
let g:go_fmt_command = "goimports"
