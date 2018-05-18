set encoding=utf-8
set guifont=InputMono\ ExLight:h24,monofur:h24,Fira_Code:h22,Input:h18,Input_Mono:h18,Fira_Code:h18

" Disable menu bollocks
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=R
set guioptions-=L
set guioptions-=l

let g:themeswitch_day='paramount'
let g:themeswitch_night='jellybeans'

execute 'colorscheme ' . g:themeswitch_night
hi! link SignColumn LineNr
hi! link htmlItalic Comment
