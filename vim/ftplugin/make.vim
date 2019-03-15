setlocal noexpandtab

if exists('b:undo_ftplugin')
    let b:undo_ftplugin .= '|noexpandtab<'
else
    let b:undo_ftplugin = '|noexpandtab<'
endif
