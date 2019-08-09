command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
command! Scratch edit ~/.scratch | normal G
command! NOH silent! /aksjdkajsd<CR>
command! CD exec "cd ".expand("%:h")
command! RMD exec "!rm ".expand("%") | bp | bd #
command! Notes edit ~/Dropbox/notes/notes.md | normal G
command! Logbook exec "e " . expand(strftime("~/Dropbox/notes/logbook/%Y/%m/%Y-%m-%d.md")) | normal G

