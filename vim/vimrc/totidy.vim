" Open Relative Markdown Links {{{1
fu! OpenLink()
    :call pandoc#hypertext#OpenLink( g:pandoc#hypertext#edit_open_cmd )
endfunction

nnoremap grl vi]y/\[<C-R>"\]<CR>f:W:call OpenLink()<cr>N:noh<cr>

" Latex / Vimtex {{{1
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
let g:tex_flavor = "latex"
let g:vimtex_indent_enabled=1
let g:vimtex_fold_enabled=1

" C++ {{{1
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

" Python {{{1
let g:pymode_python = 'python3'

let g:syntastic_python_python_exec = '/usr/local/bin/python3'
let g:syntastic_python_checkers = ['flake8']

" Rust {{{1
let g:racer_cmd = "/Users/davison/prog/z__NOT_MINE/racer/target/release/racer"
let $RUST_SRC_PATH="/Users/davison/prog/z__NOT_MINE/rust_1.3_src/src/"


" Unite ----- {{{1
let g:unite_prompt = '➜ '
if executable('ag')
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nocolor --nogroup -S -C4'
  let g:unite_source_grep_recursive_opt=''
endif
"
" The Silver Searcher (Ag) for grep {{{1
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  " let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " " ag is fast enough that CtrlP doesn't need to cache
  " let g:ctrlp_use_caching = 0
endif
if executable('rg')
  set grepprg=rg\ --vimgrep
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
endif

" bind K to grep word under cursor {{{1
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
" set up coolguy arrow prompt
"
" bind \ (backward slash) to grep shortcut
"command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap <Leader>g :Ag<SPACE>

" Mappings to easily modify vimrc {{{1
nnoremap <leader>ev :e $MYVIMRC<Cr>GO
nnoremap <leader>sv :so $MYVIMRC<Cr>
nnoremap <leader>V :e $HOME/.vim/vimrc/totidy.vim<Cr>GO
iabbrev @@ c.jr.davison@gmail.com

nnoremap <silent>/ /\v

" Uppercase the previous WORD while in normal mode {{{1

nnoremap <c-u> viwUE
