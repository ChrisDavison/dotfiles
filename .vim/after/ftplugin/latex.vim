setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
setlocal foldmethod=expr
setlocal foldexpr=vimtex#fold#level(v:lnum)
setlocal foldtext=vimtex#fold#text()
setlocal fillchars=fold:\  

let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'

command! Todos :grep \\\\todo
command! BTodos :grep \\\\todo %<CR>
