autocmd BufNewFile,BufReadPost *.tex set filetype=tex
autocmd FileType c       set foldmethod=syntax
autocmd FileType cpo     set foldmethod=syntax
autocmd FileType arduino set foldmethod=syntax
autocmd FileType python  set foldmethod=indent
autocmd FileType python  set tabstop=4
autocmd FileType python  set softtabstop=4
autocmd FileType json    set tabstop=2
autocmd FileType json    set softtabstop=2
autocmd FileType json    set shiftwidth=2
autocmd FileType go      set nofen
autocmd FileType make    set noexpandtab
autocmd FileType rust    set foldmethod=syntax
autocmd FileType vim     set foldmethod=marker
autocmd BufNewFile,BufReadPost *.es6 set filetype=javascript

let b:javascript_fold=1

