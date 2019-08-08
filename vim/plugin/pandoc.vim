augroup pandoc
    autocmd!
    au Filetype pandoc setlocal tw=80
    au Filetype pandoc setlocal foldmethod=expr
    au Filetype pandoc setlocal equalprg=pandoc\ --to\ markdown-shortcut_reference_links+pipe_tables-simple_tables\ --columns=80\ --reference-links\ --reference-location=section\ --atx-headers
    au Filetype pandoc setlocal nospell 
    " au Filetype pandoc :lchdir %:p:h
    au Filetype pandoc nnoremap D dip
augroup END

let g:pandoc#folding#fdc=0
let g:pandoc#formatting#mode="hA"
let g:pandoc#formatting#textwidth=80
let g:pandoc#spell#enabled=0
let g:pandoc#hypertext#autosave_on_edit_open_link=1
let g:pandoc#hypertext#create_if_no_alternates_exists=1
let g:pandoc#formatting#smart_autoformat_on_cursormoved=0
let g:pandoc#formatting#equalprg="pandoc --to markdown-shortcut_reference_links+pipe_tables-simple_tables --columns=81"
let g:pandoc#formatting#extra_equalprg="--reference-links --reference-location=section --atx-headers"
let g:pandoc#syntax#style#use_definition_lists=0
let g:pandoc#syntax#conceal#use=0
let g:pandoc#syntax#conceal#blacklist=['subscript', 'superscript', 'list', 'atx', 'ellipses', 'codeblock_start', 'codeblock_delim']
let g:pandoc#toc#close_after_navigating=0
let g:pandoc#syntax#conceal#use=1

nnoremap <silent><buffer> <C-n>      :ThesisNotes<CR>

iabbrev CITE ^[cite -]<LEFT>

if exists('g:loaded_toggleconceal')
    call ToggleConceal()
    call ToggleConceal()
endif
