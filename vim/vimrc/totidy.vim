" Open Relative Markdown Links {{{
function! OpenLink()
    :call pandoc#hypertext#OpenLink( g:pandoc#hypertext#edit_open_cmd )
endfunction

nnoremap grl vi]y/\[<C-R>"\]<CR>f:W:call OpenLink()<cr>N:noh<cr>
" }}}

" Latex / Vimtex {{{
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
let g:tex_flavor = "latex"
let g:vimtex_indent_enabled=1
let g:vimtex_fold_enabled=1
" }}}

" C++ {{{
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'
" }}}

" Python {{{
let g:pymode_python = 'python3'

let g:syntastic_python_python_exec = '/usr/local/bin/python3'
let g:syntastic_python_checkers = ['flake8']
" }}}

" Rust {{{
let g:racer_cmd = "/Users/davison/prog/z__NOT_MINE/racer/target/release/racer"
let $RUST_SRC_PATH="/Users/davison/prog/z__NOT_MINE/rust_1.3_src/src/"
" }}}

" The Silver Searcher (Ag) for grep {{{
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif
if executable('rg')
  set grepprg=rg\ --vimgrep
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
endif

nnoremap <Leader>g :Ag<SPACE>
" }}}

" Uppercase the previous WORD while in normal mode {{{1
nnoremap <c-u> viwUE
" }}}


autocmd! BufWritePre *.md %s/\s+$//e

function! OpenScopesSnippets()
    let ft = &filetype
    let dr = expand('~/.vim/snippets/')
    let fn = dr . ft . '.snippets'
    execute "e " . fn
endfunction
nnoremap <leader>os mZ:call OpenScopesSnippets()<Cr>
