setlocal tw=80
setlocal colorcolumn=80
setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80

if exists('b:undo_ftplugin')
    let b:undo_ftplugin .= '|tw< colorcolumn< equalprg<'
else
    let b:undo_ftplugin = '|tw< colorcolumn< equalprg<'
endif

let g:tex_flavor = "latex"
let g:vimtex_fold_enabled=1
let g:vimtex_compiler_progname='nvr'

nnoremap <buffer> <C-n>      :ThesisNotes<CR>