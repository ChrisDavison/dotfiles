" C {{{
augroup filetype_c
    autocmd!
    autocmd FileType c       set foldmethod=syntax
    autocmd FileType cpo     set foldmethod=syntax
    autocmd FileType arduino set foldmethod=syntax
augroup END
" }}}

" Python {{{
augroup filetype_py
    autocmd!
    autocmd FileType python  set foldmethod=indent
    autocmd FileType python  set tabstop=4
    autocmd FileType python  set softtabstop=4
augroup END
" }}}

" JSON {{{
augroup filetype_json
    autocmd!
    autocmd FileType json    set tabstop=2
    autocmd FileType json    set softtabstop=2
    autocmd FileType json    set shiftwidth=2
augroup END
" }}}

" Golang {{{
augroup filetype_go
    autocmd!
    autocmd FileType go      set nofen
    autocmd FileType go      set foldmethod=syntax
augroup END
" }}}

" Pandoc (markdown) {{{
augroup pandoc
    autocmd!
    autocmd Filetype markdown,pandoc setlocal wrap textwidth=80
    autocmd Filetype markdown,pandoc setlocal conceallevel=2
    autocmd Filetype markdown,pandoc hi Conceal cterm=NONE ctermbg=NONE
    autocmd Filetype markdown,pandoc hi Conceal guibg=NONE guifg=NONE
    autocmd BufEnter *.md setlocal foldexpr=MarkdownLevel()
    autocmd BufEnter *.md setlocal foldmethod=expr
augroup END

" Open Relative Markdown Links {{{2
" function! OpenRelativeMarkdownLink()
"     normal vi]y
"     normal /\[<C-R>"\]:
"     normal f:W
"     normal :call pandoc#hypertext#OpenLink( g:pandoc#hypertext#edit_open_cmd )<Cr>
"     normal N:noh<Cr>
" endfunction

"nnoremap grl vi]y/\[<C-R>"\]<CR>f:W:call OpenLink()<cr>N:noh<cr>
" }}}

" Function for markdown folding {{{2
function! MarkdownLevel()
    let h = matchstr(getline(v:lnum), '^#\+')
    if empty(h)
        return "="
    endif
    return ">" . len(h)
endfunction
" }}}

" Generate a MD preview for the current file {{{2
function! MDPreview()
    silent !clear
    let frm = '--from markdown_github+yaml_metadata_block+raw_html'
    let cfg = '--toc --toc-depth=2 --mathjax -s --self-contained'
    let style = '-c ~/.dotfiles/github-markdown.css'
    let out = '-o ~/.mdpreview.html'
    let str = '!pandoc %' . ' ' . frm . ' ' . cfg . ' ' . style . ' ' . out
    " echo str
    execute str
endfunction
" }}}

" Tidy up the current markdown file {{{2
function! MDTidy()
    silent !clear
    let ext = 'markdown+yaml_metadata_block+tex_math_dollars+line_blocks'
    let to = '--to=' . ext
    let extra = '--atx-headers --wrap=None --normalize --standalone'
    let out = '-o %'
    let mdtidy_command = 'pandoc % ' . to . ' ' . extra . ' ' . out
    execute "!" . mdtidy_command
endfunction

function! MDTidyWrap()
    silent !clear
    let ext = 'markdown+yaml_metadata_block+tex_math_dollars+line_blocks'
    let to = '--to=' . ext
    let extra = '--atx-headers --columns=80 --normalize --standalone'
    let out = '-o %'
    let mdtidy_command = 'pandoc % ' . to . ' ' . extra . ' ' . out
    execute "!" . mdtidy_command
endfunction
" }}}

" Convert current markdown file to PDF {{{2
function! MDToPDF()
    silent !clear
    let outfn=expand('%:r') . '.pdf'
    let cmd = 'pandoc % -o ' . outfn
    execute "!" . cmd
endfunction
" }}}

command! MDTidy call MDTidyWrap()
command! MDToPDF call MDToPDF()
command! MDPreview call MDPreview()
" }}}

" Miscellany {{{
augroup filetype_miscellany
    autocmd!
    autocmd BufNewFile,BufReadPost *.tex set filetype=tex
    autocmd FileType make    set noexpandtab
    autocmd FileType rust    set foldmethod=syntax
    autocmd FileType vim     set foldmethod=marker
    autocmd BufNewFile,BufReadPost *.es6 set filetype=javascript
    autocmd BufEnter * hi vimOper cterm=NONE ctermbg=NONE
    autocmd BufEnter * hi vimOper guibg=NONE guifg=NONE
augroup END
" }}}

" Settings --- 'let' commands {{{
let b:javascript_fold=1
let g:go_fmt_command = "goimports"

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


let g:pandoc#spell#enabled=0
let g:pandoc#syntax#conceal#urls = 1
let g:pandoc#formatting#mode='s'
let g:pandoc#formatting#textwidth=0
let g:pandoc#formatting#equalprg = "pandoc -t markdown -s"
let g:pandoc#formatting#extra_equalprg = "--columns=80 --normalize --atx-headers"
let g:pandoc#syntax#conceal#blacklist = ['list', 'atx']
let g:vim_markdown_toc_autofit = 1
" }}}

