" Markdown ----- {{{1
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

autocmd Filetype markdown setlocal wrap textwidth=80
autocmd Filetype markdown setlocal conceallevel=2
autocmd Filetype markdown hi Conceal cterm=None ctermbg=None
let g:scratch_filetype = 'pandoc'


let g:pandoc#spell#enabled=0
let g:pandoc#syntax#conceal#urls = 1
let g:pandoc#formatting#mode='ha'
let g:pandoc#formatting#textwidth=80
let g:pandoc#formatting#equalprg = "pandoc -t markdown --reference-links"
let g:pandoc#formatting#extra_equalprg = "--wrap=auto --normalize --atx-headers"


