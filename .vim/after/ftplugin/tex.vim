setlocal equalprg=pandoc\ --from\ latex\ --to\ latex\ --columns=80
setlocal foldmethod=expr
setlocal foldexpr=vimtex#fold#level(v:lnum)
setlocal foldtext=vimtex#fold#text()
setlocal fillchars=fold:\  

let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'

command! Todos :grep \\\\todo<CR>
command! BTodos :grep \\\\todo %<CR>
command! Tables :grep '\\\\begin\{table'<CR>
command! BTables :grep '\\\\begin\{table' %<CR>
command! Figures :grep '\\\\begin\{figure'<CR>
command! BFigures :grep '\\\\begin\{figure' %<CR>
