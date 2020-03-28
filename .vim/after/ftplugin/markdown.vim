let g:markdown_fenced_languages = ['python', 'rust', 'cpp', 'go']
let g:pandoc#formatting#mode='hA'
let g:pandoc#keyboard#use_default_mappings=0
let g:pandoc#formatting#smart_autoformat_on_cursormoved=1

let g:markdown_reference_links=0
let g:markdown_hard_wrap=1

if g:markdown_hard_wrap
    setlocal formatoptions+=a
    setlocal textwidth=79
endif

let md_equalprg="pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block"
let md_equalprg.=g:markdown_reference_links ? "-shortcut_reference_links\ --reference-links\ --reference-location=section" : ""
let md_equalprg.=g:markdown_hard_wrap ? "\ --columns=79\ --wrap=auto" : "\ --wrap=none"
let md_equalprg.="\ --atx-headers"

let &l:equalprg=md_equalprg
let g:pandoc#formatting#equalprg=md_equalprg
let g:pandoc#formatting#extra_equalprg=''
let g:pandoc#modules#disabled = ['hypertext', 'spell']
let g:pandoc#folding#fdc=0
let g:pandoc#folding#fold_fenced_codeblocks=1
let g:pandoc#syntax#conceal#urls=0
let g:pandoc#syntax#conceal#blacklist=['ellipses', 'atx', 'subscript', 'superscript', 'strikeout', 'codeblock_start', 'codeblock_delim', 'footnote', 'definition', 'list']
let g:pandoc#spell#enabled=0

setlocal foldenable
setlocal foldlevelstart=1
setlocal foldmethod=expr
" setlocal foldexpr=markdown#fold_level()
setlocal conceallevel=1
setlocal nospell

" au BufLeave *.txt,*.md call markdown#copy_filename_as_link()
" au BufLeave *.txt,*.md exec "let @a=" . expand("@")

command! -bang Backlinks call markdown#backlinks(<bang>1)
nnoremap <leader>B :Backlinks!<CR>

" .vim/autoload/file.vim
nnoremap ml :call markdown#file_from_selection(0)<CR>
vnoremap ml :call markdown#file_from_selection(1)<CR>
nnoremap gml :call markdown#file_from_selection_and_edit(0)<CR>
vnoremap gml :call markdown#file_from_selection_and_edit(1)<CR>
nnoremap gf :call markdown#goto_file(0)<CR>
nnoremap gs :call markdown#goto_file(2)<CR>
nnoremap <leader>gf :call markdown#goto_file(0)<CR>
nnoremap <leader>gs :call markdown#goto_file(1)<CR>

let g:forced_plaintext_files=['calendar.txt', 'todo.txt', 'shopping.txt', 'done.txt', 'daily.txt', 'weekly.txt', 'monthly.txt']

