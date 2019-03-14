command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
command! Scratch edit ~/.scratch | normal G
command! NOH silent! /aksjdkajsd<CR>
command! AddPlugin exec 'e '.globpath(&rtp, '*/junegunn_plug_plugins.vim') | normal GOPlug ''<LEFT>
