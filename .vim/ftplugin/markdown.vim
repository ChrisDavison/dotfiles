let md_wrap=' --columns=80 --wrap=auto'
let md_nowrap=' --wrap=none'
let md_reflinks=' --reference-links'
let md_reflinks=' --reference-links --reference-location=section'
let md_standalone=" --standalone"
let md_equalprg="pandoc --to markdown+smart-simple_tables+pipe_tables --markdown-headings=atx"
let md_equalprg .= md_wrap . md_standalone

let &l:equalprg=md_equalprg

let g:pandoc#keyboard#use_default_mappings=0
let g:pandoc#formatting#mode='ha'
let g:pandoc#formatting#smart_autoformat_on_cursormoved=0
let g:pandoc#formatting#equalprg=md_equalprg
let g:pandoc#formatting#extra_equalprg=''
let g:pandoc#formatting#textwidth=80
let g:pandoc#folding#fdc=0
let g:pandoc#folding#fold_fenced_codeblocks=1
let g:pandoc#syntax#conceal#use=1
let g:pandoc#spell#enabled=0
let g:pandoc#toc#position="left"
let g:pandoc#toc#close_after_navigating=0

setlocal noautoindent
setlocal nospell 
setlocal conceallevel=2
setlocal formatoptions-=a textwidth=80 formatoptions+=n

" Defined in ~/.vim/autoload/markdown.vim
command! -bang Backlinks call markdown#backlinks(<bang>1)
command! FilenameAsHeader call markdown#filename_as_header()
command! HeaderDecrease call markdown#header_decrease()
command! HeaderIncrease call markdown#header_increase()

command! CheckboxForward s/\[ \]/**[»]**/

vnoremap <buffer> <leader>w :call markdown#move_visual_selection_to_file()<CR>
nnoremap <buffer> gf :call markdown#goto_file(0)<CR>
nnoremap <buffer> gs :call markdown#goto_file(1)<CR>
nnoremap <leader>S :call markdown#new_section()<CR>

" Keybinds for pandoc, as I've disabled the defaults
nnoremap <buffer> ]] :call pandoc#keyboard#sections#NextHeader()<CR>
nnoremap <buffer> [[ :call pandoc#keyboard#sections#PrevHeader()<CR>
vmap <buffer> aS <Plug>(pandoc-keyboard-select-section-inclusive)
omap <buffer> aS :normal VaS<CR>
vmap <buffer> iS <Plug>(pandoc-keyboard-select-section-exclusive)
omap <buffer> iS :normal ViS<CR>
