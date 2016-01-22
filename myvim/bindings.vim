" Colon used a lot more often than semicolon ----- {{{1
nnoremap ; :
nnoremap : :

" EX mode is a pain ----- {{{1
map q: :q

" View and switch to buffer ----- {{{1
nnoremap gb :ls<CR>:buffer<Space>
nnoremap bd :ls<CR>:bd<Space>

" Indent/De-dent visual selection ----- {{{1
vnoremap < <gv
vnoremap > >gv


" Split/Window management ----- {{{1
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

" Move by VISUAL lines ----- {{{1
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$

" For various useful plugins ----- {{{1
nnoremap <leader>w :w<CR>
nnoremap <leader>v :VimtexTocToggle<CR>
"nnoremap <leader>c :SyntasticCheck<CR>

" Toggle whitespace visibility with ,s ----- {{{1
nmap <Leader>s :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬


" Easier search/replace ----- {{{1
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s///g<LEFT><LEFT>
vmap S :s///g<LEFT><LEFT>

" Common mistypes and abbreviations ----- {{{1
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa


" Pasting ----- {{{1
" Paste and move to end
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Select what was pasted
noremap gV `[v`]
