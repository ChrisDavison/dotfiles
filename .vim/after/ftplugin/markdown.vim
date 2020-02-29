let g:markdown_fenced_languages = ['python', 'rust', 'cpp', 'go']

let g:markdown_reference_links=0
let g:markdown_hard_wrap=0

let md_equalprg="pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block"
let md_equalprg.=g:markdown_reference_links ? "-shortcut_reference_links\ --reference-links\ --reference-location=section" : ""
let md_equalprg.=g:markdown_hard_wrap ? "\ --columns=79\ --wrap=auto" : "\ --wrap=none"
let md_equalprg.="\ --atx-headers"

let &l:equalprg=md_equalprg

setlocal foldenable
setlocal foldlevelstart=99
setlocal foldmethod=expr
setlocal foldexpr=markdown#fold_level()
setlocal conceallevel=1
setlocal nospell
let g:pandoc#syntax#conceal#urls=1

au BufLeave *.txt,*.md call markdown#copy_filename_as_link()

command! -bang Backlinks call markdown#backlinks(<bang>1)
nnoremap <leader>B :Backlinks!<CR>

" .vim/autoload/file.vim
nnoremap ml :call markdown#file_from_selection(0)<CR>
vnoremap ml :call markdown#file_from_selection(1)<CR>
nnoremap gml :call markdown#file_from_selection_and_edit(0)<CR>
vnoremap gml :call markdown#file_from_selection_and_edit(1)<CR>
nnoremap gf :call markdown#goto_file()<CR>
