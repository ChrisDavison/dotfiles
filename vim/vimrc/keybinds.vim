" Binding ----- {{{1
" Use '//' in visual mode to search for selection
vnoremap // y/<C-R>"<CR>
"
" <leader>e -- edit file, starting in same directory as current file
nmap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Swap colon and semicolon
nnoremap ; :
nnoremap : :

" Use spacebar for folds
nnoremap <space> za

" EX mode is a pain
map q: :q

" View and switch to buffer
nnoremap gb :ls<CR>:buffer<Space>

" Indent/De-dent visual selection
vnoremap < <gv
vnoremap > >gv


" Split/Window management
" Move windows
map <C-w><C-h> <C-w><S-h>
map <C-w><C-j> <C-w><S-j>
map <C-w><C-k> <C-w><S-k>
map <C-w><C-l> <C-w><S-l>

" Move BETWEEN windows
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>

" Unite
nnoremap <leader>p :Unite buffer -no-split<cr>
nnoremap <leader>o :Unite outline -no-split<cr>
nnoremap - :Unite file -no-split<cr>

" Move by VISUAL lines
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$

" Toggle Vimtex Table of Contents
nnoremap <leader>v :VimtexTocToggle<CR>

" Toggle tagbar
nnoremap <leader>t :TagbarToggle<CR>

" Toggle hidden character visibility with
nmap <Leader>h :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬


" Easier search/replace
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s//g<LEFT><LEFT>
vmap S :s//g<LEFT><LEFT>

" Common mistypes and abbreviations
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa


" Selecting and Pasting
" Paste and move to end
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Select what was pasted
noremap gV `[v`]

nnoremap <leader>a ggVG

" Generate a MD preview for the current file
nnoremap mp :!pandoc -s -c ~/.dotfiles/simple-pandoc-css.css % -o ~/.mdpreview.html<CR><CR>

" Keychords to quickly navigate quickfix list
nnoremap cn :cn<CR>
nnoremap cp :cp<CR>


nmap <leader>s <plug>(scratch-insert-reuse)

nnoremap <silent> <leader>/ :nohlsearch<CR>
