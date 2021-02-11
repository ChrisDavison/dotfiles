let md_wrap=' --columns=80 --wrap=auto'
let md_nowrap=' --wrap=none'
let md_reflinks=' --reference-links'
let md_reflinks=' --reference-links --reference-location=section'
let md_standalone=" --standalone"
let md_equalprg="pandoc --to markdown+smart --markdown-headings=atx"
let md_equalprg .= md_wrap . md_standalone

let g:pandoc#keyboard#use_default_mappings=0
let g:pandoc#formatting#mode='hA'
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

let &l:equalprg=md_equalprg
setlocal noautoindent
setlocal nospell 
setlocal conceallevel=2
setlocal formatoptions-=a textwidth=80 formatoptions+=n

" Insert a new 'section' (L2 markdown header)
nnoremap <leader>S :call append(line('$'), ['','## '])<CR>:norm G<CR>:startinsert!<CR>

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
        if len(getqflist()) == 0
            exec "cclose"
        endif
    else
        call fzf#vim#grep(
        \ "rg --column --line-number --no-heading --color=always --smart-case -g '!tags' ".expand('%'), 1,
        \ fzf#vim#with_preview('right:50%:hidden', '?'), 0)
    end
endfunction " 
command! -bang Backlinks call Markdown_backlinks(<bang>1)
" }}}
" Move visual selection to another file {{{
function! s:move_visual_selection_to_file()
    let filename=input("Filename: ")
    let filename_nospace=substitute(l:filename, ' ', '-', 'g')
    exec ":'<,'>w " . l:filename_nospace . ".md"
    exec ":normal gvD"
endfunction

vnoremap <buffer> <leader>w :call <sid>move_visual_selection_to_file()<CR>
" }}}
" Don't highlight code blocks
" This is a hack to prevent indented lists from displaying as code blocks

nnoremap <buffer> ]] :call pandoc#keyboard#sections#NextHeader()<CR>
nnoremap <buffer> [[ :call pandoc#keyboard#sections#PrevHeader()<CR>
vmap <buffer> aS <Plug>(pandoc-keyboard-select-section-inclusive)
omap <buffer> aS :normal VaS<CR>
vmap <buffer> iS <Plug>(pandoc-keyboard-select-section-exclusive)
omap <buffer> iS :normal ViS<CR>
