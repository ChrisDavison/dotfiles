set equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80

let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'

command! Todos :grep \\\\todo
command! BTodos :grep \\\\todo %<CR>
