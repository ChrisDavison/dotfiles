let b:javascript_fold=1

setlocal foldmethod=syntax

if exists('b:undo_ftplugin')
    let b:undo_ftplugin .= '|foldmethod<'
else
    let b:undo_ftplugin = '|foldmethod<'
endif 