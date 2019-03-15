cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e

cnoreabbrev Q! q!

iabbrev undoftp if exists('b:undo_ftplugin')
            \<CR>let b:undo_ftplugin .= ''
            \<CR>else
            \<CR>let b:undo_ftplugin = ''
            \<CR>endif

