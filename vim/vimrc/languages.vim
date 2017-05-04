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

" Markdown ----- {{{
"
" Tagbar support
" Add support for markdown files in tagbar.
let g:tagbar_type_markdown = {
    \ 'ctagstype': 'markdown',
    \ 'ctagsbin' : '/Users/davison/prog/z__NOT_MINE/markdown2ctags/markdown2ctags.py',
    \ 'ctagsargs' : '-f - --sort=yes',
    \ 'kinds' : [
        \ 's:sections',
        \ 'i:images'
    \ ],
    \ 'sro' : '|',
    \ 'kind2scope' : {
        \ 's' : 'section',
    \ },
    \ 'sort': 0,
\ }

" Tables
let g:table_mode_corner="|"
let g:table_mode_corner_corner="|"
let g:table_mode_header_fillchar="-"

let g:tex_flavor='latex'

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpForwardTrigger="<c-z>"

let g:UltiSnipsEditSplit="vertical"

autocmd Filetype markdown,pandoc setlocal wrap textwidth=80
autocmd Filetype markdown,pandoc setlocal conceallevel=2
autocmd Filetype markdown,pandoc hi Conceal cterm=None ctermbg=None
let g:scratch_filetype = 'pandoc'

let g:pandoc#spell#enabled=0
let g:pandoc#syntax#conceal#urls = 1
let g:pandoc#formatting#mode='ha'
let g:pandoc#formatting#textwidth=80
let g:pandoc#formatting#equalprg = "pandoc -t markdown -s"
let g:pandoc#formatting#extra_equalprg = "--columns=80 --normalize --atx-headers"
"}}}
