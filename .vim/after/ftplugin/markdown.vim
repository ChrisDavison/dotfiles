let md_wrap=' --columns=72 --wrap=auto'
let md_nowrap=' --wrap=none'
let md_reflinks=' --reference-links'
" let md_reflinks=' --reference-links --reference-location=section'

let md_equalprg="pandoc --to markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block-shortcut_reference_links --atx-headers"
let md_equalprg .= md_wrap . md_reflinks

let g:voom_default_mode="pandoc"
let g:voom_tree_width=50
let g:markdown_folding=1

let &l:equalprg=md_equalprg
setlocal noautoindent
setlocal foldenable foldmethod=expr foldlevelstart=1 
setlocal nospell 
setlocal conceallevel=2
setlocal formatoptions+=a textwidth=72
nnoremap <buffer> <leader>i :g/^#/:p<CR>:

command! H1 g/^#\{1,1\} /
command! H2 g/^#\{1,2\} /
command! H3 g/^#\{1,3\} /
command! FMT normal mzggvG=`z

" fn: Goto file {{{
function! Markdown_goto_file(split)
    let fname=expand("<cfile>")
    let command = "edit "
    if a:split > 0
        if winwidth(0) > 160
            let command = "vsplit "
        else
            let command = "split "
        endif
    endif
    if filereadable(l:fname)
        execute "silent!" . l:command . l:fname
    else
        if getline(".")[col(".")] != "]"
            normal f]
        end
        normal vi("by
        if filereadable(getreg("b"))
            execute "silent!" . l:command . getreg("b")
        else
            echom "Couldn't find valid link."
        end
    end
endfunction " 
nnoremap <buffer> gf :call Markdown_goto_file(0)<CR>
nnoremap <buffer> gs :call Markdown_goto_file(2)<CR>
" }}}
" fn: Backlinks - files that link TO this file {{{
function! Markdown_backlinks(use_grep)
    if a:use_grep
        exec "silent grep! '\\((\./)*" . expand("%") . "'"
    else
        call fzf#vim#grep(
        \ "rg --column --line-number --no-heading --color=always --smart-case -g '!tags' ".expand('%'), 1,
        \ fzf#vim#with_preview('right:50%:hidden', '?'), 0)
    end
endfunction " 
command! -bang Backlinks call Markdown_backlinks(<bang>1)
" }}}
" Don't highlight code blocks
" This is a hack to prevent indented lists from displaying as code blocks
highlight clear markdownCode
highlight clear markdownCodeBlock
