let g:cd_prefer_fzf = 1

" Split/Window management {{{
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

" }}}

" Move by VISUAL lines {{{
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$
" }}}

" Quicker search/replace {{{
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s//g<LEFT><LEFT>
vmap S :s//g<LEFT><LEFT>
" }}}

" Selecting and Pasting {{{
" Paste and move to end
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Select what was pasted
noremap gV `[v`]

" Select ALL
nnoremap <leader>a ggVG
" }}}

" Navigate quickfix with keychords {{{
nnoremap cn :cn<CR>
nnoremap cp :cp<CR>
" }}}

" Buffer/File/Function/Outline navigation (CtrlP versus FZF) {{{
" Unite
nnoremap <leader>p :Unite buffer -no-split<cr>
nnoremap <leader>o :Unite outline -no-split<cr>
nnoremap - :Unite file -no-split<cr>

if get(g:, 'cd_prefer_fzf', 0)
    nnoremap <leader>b :Buffers<Cr>
    nnoremap <leader>p :Files<Cr>
else
    nnoremap <leader>b  :CtrlPBuffer<Cr>
    nnoremap <leader>p  :CtrlP<Cr>
endif

" Funky (CtrlP for functions)
nnoremap <leader>fu :CtrlPFunky<Cr>
nnoremap <leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
" }}}

" Keybinds to manipulate my vim config {{{
nnoremap <leader>ev :e $MYVIMRC<Cr>
nnoremap <leader>sv :so $MYVIMRC<Cr>
nnoremap <leader>et :e ~/.vim/vimrc/totidy.vim<Cr>GO
nnoremap <leader>V :e ~/.vim/vimrc/
" }}}

" Miscellany {{{
" Toggle hidden character visibility with
nmap <Leader>h :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬

" Toggle Vimtex Table of Contents
" nnoremap <leader>v :VimtexTocToggle<CR>

" Toggle tagbar
nnoremap <leader>t :TagbarToggle<CR>

" Generate a MD preview for the current file
nnoremap mp :!pandoc -s -c ~/.dotfiles/simple-pandoc-css.css % -o ~/.mdpreview.html<CR><CR>

nmap <leader>s <plug>(scratch-insert-reuse)

nnoremap <silent> <leader>/ :nohlsearch<CR>

" Use '//' in visual mode to search for selection
vnoremap // y/<C-R>"<CR>

nnoremap / /\v

" <leader>e -- edit file, starting in same directory as current file
" perhaps not needed...using autochdir, so ':e' will use curdir
" nmap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

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

" Fold HTML tags
nnoremap <leader>ft Vatzf

" Format a paragraph
nnoremap <leader>q gqip
" }}}
