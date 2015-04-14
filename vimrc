" Suggested headlines for configuration organisation

"" Appearance
"" Writing
"" Languages
"" Movement / window management
"" Filetype/syntax management
"" Utility
" General setup
" -------------
set nocompatible
syntax on
filetype plugin indent on
set encoding=utf-8
set showcmd
set wrap lbr
set omnifunc=syntaxcomplete#Complete
set number
set iskeyword=a-z,A-Z,_,.,39
set rtp+=~/.fzf
" Plugins, managed with JuneGunn's vim plugged
" --------------------------------------------
call plug#begin('~/.vim/plugged')

Plug 'Blackrush/vim-gocode'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go'
Plug 'greyblake/vim-preview'
Plug 'jceb/vim-orgmode'
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'kien/ctrlp.vim'
Plug 'lervag/vimtex'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-markdown'
Plug 'rizzatti/dash.vim'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'sjl/gundo.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/paredit.vim'
Plug 'wting/rust.vim'
Plug 'wlangstroth/vim-racket'

call plug#end()
" Increase/decrease font size
command! -bar -nargs=0 Bigger  :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)+1','')
command! -bar -nargs=0 Smaller :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)-1','')
noremap \fd        :Smaller<CR>
noremap \fu        :Bigger<CR>
" MY KEY BINDINGS
map <space> /
imap ii <esc>
map vv <C-w>v
map vn <C-w>n
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
nnoremap \c :SyntasticCheck<CR>
"nnoremap \m :!Make<CR> "Doesn't find current path...
" CtrlP config
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
let g:ctrlp_working_path_mode = ''
" Map to visible rather than literal lines
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$
" Use Ctrl-HJKL to switch windows
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
" Bindings to specific files
nnoremap  \gc :tabe $MYVIMRC<CR>
nnoremap  \gr :tabe ~/Dropbox/org/rust.org<CR>
nnoremap  \gu :tabe ~/Dropbox/org/uni.org<CR>
nnoremap  \gn :tabe ~/Dropbox/org/notes.org<CR>
" Search options
" --------------
set incsearch
set hlsearch
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start
" Various coding preferences
" --------------------------
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set wildmenu
set lazyredraw
set laststatus=2
set nrformats=
set title
set sidescrolloff=15
set sidescroll=1
" Some common miss-types/abbreviations
" ------------------------------------
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa
" Put all temp files in one place
" -------------------------------
set backup
set backupdir=~/.vim/backup,.
set directory=~/.vim/tmp,.
" My colour scheme
" ----------------
set bg=dark
colorscheme seoul256
set t_ut=
" Highlight a character in the 81st column
" ----------------------------------------
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)
" Allow markdown preview
" ----------------------
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
let g:PreviewCSSPath='http://www.chrisdavison.org/assets/md_preview.css'
let g:PreviewBrowsers='google-chrome,google-chrome-stable,safari,firefox'
" Goyo (distractionfree) and Limelight (ENHANCED distractionfree)
" ---------------------------------------------------------------
let g:goyo_width=100
let g:goyo_margin_top=1
let g:goyo_margin_bottom=1
autocmd User GoyoEnter Limelight
autocmd User GoyoLeave Limelight!
" Code folding..yes or no?
" ------------------------
set foldenable
"set nofoldenable
" Associate filetypes
" -------------------
autocmd BufRead,BufNewFile *.rs set syntax=rust
autocmd BufRead,BufNewFile *.md set filetype=markdown
" <F8> | Color scheme rotator
" ---------------------------
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
" Easy alignment mode
" -------------------
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
" Rainbow parentheses
autocmd VimEnter * RainbowParentheses

