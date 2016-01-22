"  Latex / Vimtex ----- {{{1
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
let g:tex_flavor = "latex"
autocmd BufWritePre *.tex :VimtexRefreshFolds

"  C++ ----- {{{1
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

" Python ----- {{{1
let g:pymode_python = 'python3'

let g:syntastic_python_python_exec = '/usr/local/bin/python3'

" Rust ----- {{{1
let g:racer_cmd = "/Users/davison/prog/z__NOT_MINE/racer/target/release/racer"
let $RUST_SRC_PATH="/Users/davison/prog/z__NOT_MINE/rust_1.3_src/src/"

