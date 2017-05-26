set bg=dark
colorscheme paramount
set t_ut=

"au BufReadPost * set relativenumber
set gfn=Iosevka:h18

" " Highlight long lines
" highlight OverLength ctermbg=red ctermfg=white
" match OverLength /\%81v.\+/

"-- Highlight long rows ----- {{{2
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

