let g:rustfmt_autosave = 1
setlocal foldmethod=syntax

"if executable('rls')
"    au User lsp_setup call lsp#register_server({
"                \ 'name': 'rls',
"                \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
"                \ 'whitelist': ['rust'],
"                \})
"endif

if exists('b:undo_ftplugin')
    let b:undo_ftplugin .= '|foldmethod<'
else
    let b:undo_ftplugin = '|foldmethod<'
endif
