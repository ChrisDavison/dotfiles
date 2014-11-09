" -------------
" General setup
" -------------
set nocompatible
syntax on
filetype plugin indent on
set wrap lbr
set omnifunc=syntaxcomplete#Complete
set number
set iskeyword=a-z,A-Z,_,.,39
set rtp+=~/.fzf

call plug#begin('~/.vim/plugged')

Plug 'Blackrush/vim-gocode'
Plug 'Lokaltog/powerline'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go'
Plug 'greyblake/vim-preview'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'kien/ctrlp.vim'
Plug 'mattn/emmet-vim'
Plug 'plasticboy/vim-markdown'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdtree'
Plug 'sjl/gundo.vim'
Plug 'szw/vim-tags'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'rizzatti/dash.vim'

call plug#end()

" ---------------
" MY KEY BINDINGS
" ---------------
map ;; <esc>
map <space> /
map vv <C-w>v
map vn :vnew<CR>
map ;. <C-w>>
map ;, <C-w><
map <C-w><C-h> <C-w><S-h>
map <C-w><C-j> <C-w><S-j>
map <C-w><C-k> <C-w><S-k>
map <C-w><C-l> <C-w><S-l>

" Bindings for various useful plugins
nnoremap \e :NERDTreeToggle<CR>
nnoremap \b :CtrlPBuffer<CR>
nnoremap \p :Preview<CR>
nnoremap \w :w !wc %<CR>
nnoremap \g :Goyo<CR>
nnoremap \u :GundoToggle<CR>
nnoremap \d :Dash<CR>

" Map to visible rather than literal lines
nnoremap  <buffer> <silent>k gk
vnoremap  <buffer> <silent>k gk
nnoremap  <buffer> <silent>j gj
vnoremap  <buffer> <silent>j gj
nnoremap  <buffer> <silent>0 g0
vnoremap  <buffer> <silent>0 g0
nnoremap  <buffer> <silent>$ g$
vnoremap  <buffer> <silent>$ g$

nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>

" ---------------------
" Execute code using F9
" ---------------------
autocmd FileType go nnoremap <F9> :!go run %<CR>
autocmd FileType python nnoremap <F9> :!python3 %<CR>

" --------------
" Search options
" --------------
set incsearch
set hlsearch
set ignorecase
set smartcase
set magic
set bs=indent,eol,start

" --------------------------
" Various coding preferences
" --------------------------
set tabstop=8
set softtabstop=8
set shiftwidth=8
set expandtab
set wildmenu
set lazyredraw
set laststatus=2
set nrformats=
set title
set sidescrolloff=15
set sidescroll=1

" ------------------------------------
" Some common miss-types/abbreviations
" ------------------------------------
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa

" -------------------------------
" Put all temp files in one place
" -------------------------------
set backup
set backupdir=~/.vim/backup,.
set directory=~/.vim/tmp,.

" ----------------
" My colour scheme
" ----------------
set bg=dark
colorscheme solarized
set t_ut=

" ---------------------------
" CtrlP plugin settings/setup
" ---------------------------

" Ag used instead of Grep as it's faster and doesn't need caching.
let g:ctrlp_working_path_mode = 0
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor
        let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
              \ --ignore .git
              \ --ignore .svn
              \ --ignore .hg
              \ --ignore .DS_Store
              \ --ignore .cabal
              \ --ignore "**/*.pyc"
              \ --ignore .cargo
              \ --ignore .cache
              \ --ignore .dotfiles
              \ -g ""'
    let g:ctrlp_use_caching = 0
    let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
endif
" ----------------------------------------
" Highlight a character in the 81st column
" ----------------------------------------
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

" if has('gui_running')
"     set gfn=Source\ Code\ Pro\ Medium\ 12
" endif
if has('gui_running')
    set gfn=Osaka-Mono:h18
endif

" Allow markdown preview
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
let g:PreviewCSSPath='http://www.chrisdavison.org/assets/md_preview.css'
let g:PreviewBrowsers='google-chrome,google-chrome-stable,safari,firefox'

" Goyo (distractionfree) and Limelight (ENHANCED distractionfree)
let g:goyo_width=100
let g:goyo_margin_top=1
let g:goyo_margin_bottom=1
autocmd User GoyoEnter Limelight
autocmd User GoyoLeave Limelight!

" Don't allow code folding
set nofoldenable

" Associate .rs filetype with rust syntax
au BufRead,BufNewFile *.rs set syntax=rust

" Disable folding for markdown
let g:vim_markdown_folding_disabled=1

" ----------------------------------------------------------------------------
" <F8> | Color scheme selector
" ----------------------------------------------------------------------------
function! s:rotate_colors()
  if !exists('s:colors_list')
    let s:colors_list =
    \ sort(map(
    \   filter(split(globpath(&rtp, "colors/*.vim"), "\n"), 'v:val !~ "^/usr/"'),
    \   "substitute(fnamemodify(v:val, ':t'), '\\..\\{-}$', '', '')"))
  endif
  if !exists('s:colors_index')
    let s:colors_index = index(s:colors_list, g:colors_name)
  endif
  let s:colors_index = (s:colors_index + 1) % len(s:colors_list)
  let name = s:colors_list[s:colors_index]
  execute 'colorscheme' name
  redraw
  echo name
endfunction
nnoremap <F8> :call <SID>rotate_colors()<cr>
