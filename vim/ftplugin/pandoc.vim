setlocal tw=80
setlocal foldmethod=expr
setlocal equalprg=pandoc\ --to\ markdown-shortcut_reference_links\ --columns=80\ --reference-links\ --atx-headers 
setlocal nospell 

if exists('b:undo_ftplugin')
    let b:undo_ftplugin.='|setlocal tw< foldmethod< equalprg< nospell<'
else
    let b:undo_ftplugin='|setlocal tw< foldmethod< equalprg< nospell<'
endif

let g:pandoc#folding#fdc=0
let g:pandoc#formatting#mode="hA"
let g:pandoc#formatting#textwidth=80
let g:pandoc#spell#enabled=0
let g:pandoc#hypertext#autosave_on_edit_open_link=1
let g:pandoc#hypertext#create_if_no_alternates_exists=1
let g:pandoc#formatting#smart_autoformat_on_cursormoved=1
let g:pandoc#formatting#equalprg="pandoc --to markdown-shortcut_reference_links --columns=80"
let g:pandoc#formatting#extra_equalprg="--reference-links --atx-headers"
let g:pandoc#toc#close_after_navigating=0
