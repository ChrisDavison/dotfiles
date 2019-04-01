command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
command! Scratch edit ~/.scratch | normal G
command! NOH silent! /aksjdkajsd<CR>
command! AddPlugin exec 'e '.globpath(&rtp, '*/junegunn_plug_plugins.vim') | normal GOPlug ''<LEFT>
command! RefreshPlugins call NewPlugins()<CR>
command! CD exec "cd ".expand("%:h")
command! RMD exec "!rm ".expand("%") | bp | bd #
command! ASMR edit ~/Dropbox/asmr.json | normal Gzz
command! Todos edit ~/Dropbox/notes/todo.md | normal Gzz
command! Dones edit ~/Dropbox/notes/done.md | normal Gzz
command! Journal edit ~/Dropbox/notes/journal.md | normal Gzz
command! Projects exec "e " . expand("~/Dropbox/notes/projects/")
