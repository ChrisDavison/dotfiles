" setlocal equalprg=pandoc\ --from\ latex\ --to\ latex\ --columns=80
setlocal foldmethod=expr
setlocal foldexpr=vimtex#fold#level(v:lnum)
setlocal foldtext=vimtex#fold#text()
setlocal fillchars=fold:\  
let g:vimtex_format_enabled=1
setlocal formatoptions-=a

let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'

command! Todos :silent!grep \\\\todo<CR>
command! BTodos :silent!lgrep \\\\todo %<CR>
command! Tables :silent!grep '\\\\begin\{table'<CR>
command! BTables :silent!lgrep '\\\\begin\{table' %<CR>
command! Figures :silent!grep '\\\\begin\{figure'<CR>
command! BFigures :silent!lgrep '\\\\begin\{figure' %<CR>
