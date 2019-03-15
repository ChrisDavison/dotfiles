setlocal foldmethod=marker

if exists('b:undo_ftplugin')
    let b:undo_ftplugin .= '|foldmethod<'
else
    let b:undo_ftplugin = '|foldmethod<'
endif
