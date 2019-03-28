command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
command! Scratch edit ~/.scratch | normal G
command! NOH silent! /aksjdkajsd<CR>
command! AddPlugin exec 'e '.globpath(&rtp, '*/junegunn_plug_plugins.vim') | normal GOPlug ''<LEFT>
command! RefreshPlugins call NewPlugins()<CR>
command! NewJournal exec "r " . expand("~/.vim_file_templates/journal.md")
command! CD exec "cd ".expand("%:h")
command! RMD exec "!rm ".expand("%") | bp | bd #
