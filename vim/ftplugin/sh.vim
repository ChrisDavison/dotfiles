let g:sh_fold_enabled=5
let g:is_bash=1

setlocal foldmethod=syntax

if exists('b:undo_ftplugin')
    let b:undo_ftplugin.='|foldmethod<'
else
    let b:undo_ftplugin='|foldmethod<'
endif
