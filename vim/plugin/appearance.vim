set t_Co=256
set bg=dark
" set termguicolors
silent! colorscheme seoul256
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

if has('windows')
    set fillchars=vert:?
    set fillchars+=fold:·
endif

