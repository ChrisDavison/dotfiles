" Author: Chris Davison
" Comments: My Vim config.  Not much language-specific stuff in this, as the
" plugin defaults seem to be pretty good in most cases.  For the record, using
" JuneGunn's 'plugged' plugin manager, as I find it's simplistic approach the
" most appealling.  If your fold is set to "marker" for the "vim" filetype,
" then this should fold in a relatively well explained manner, allowing you to
" find and change whatever you need.

" Set leader as space.  It's big, it's in the middle, and it's easy from the
" home row
let mapleader="\<Space>"

" Initial ----- {{{1
" |====  Good defaults {{{2
set nocompatible
syntax on
filetype plugin indent on
set encoding=utf-8
scriptencoding utf-8
set showcmd
set wrap lbr
set omnifunc=syntaxcomplete#Complete
set number
set iskeyword=a-z,A-Z,_,.,39

" |====  Search options {{{2
set incsearch
set hlsearch
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

" |====  Various coding preferences {{{2
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set wildmenu
set autoread
set clipboard=unnamed
set lazyredraw
set laststatus=2
set nrformats=
set title
set sidescrolloff=15
set sidescroll=1

" |====  Put all temp files in one place {{{2
set backup
set backupdir=~/.vim/backup,.
set directory=~/.vim/tmp,.

" |====  Wildmenu config {{{2
set wildmode=list:longest
set wildmenu                

set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif

" |====  Allow code folding {{{2
set foldenable
set foldmethod=marker


" Plugins ----- {{{1
" Using JuneGunn's plugged
call plug#begin('~/.vim/plugged')

Plug 'ap/vim-css-color'
Plug 'dahu/vim-fanfingtastic'
Plug 'fatih/vim-go'
Plug 'guns/vim-sexp'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'lervag/vimtex'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'mattn/emmet-vim'
Plug 'rking/ag.vim'
Plug 'plasticboy/vim-markdown'
Plug 'nvie/vim-flake8'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'Shougo/vimproc.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'wlangstroth/vim-racket'
Plug 'wting/rust.vim'

call plug#end()


" Appearance ----- {{{1
set bg=light
colorscheme github
set t_ut=

au BufReadPost * set relativenumber

" Bindings ----- {{{1
" |==== GENERAL {{{2
" Colon used a lot more often
nnoremap ; :
nnoremap : :

" EX mode is a pain
map q: :q

" View and switch to buffer
nnoremap gb :ls<CR>:buffer<Space>

" Indent/De-dent visual selection
vnoremap < <gv
vnoremap > >gv


" |==== Split/Window management {{{2
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

" |==== Move by VISUAL lines {{{2
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$

" |==== For various useful plugins {{{2
nnoremap <leader>e :NERDTreeToggle<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>v :VimtexTocToggle<CR>
nnoremap <leader>c :SyntasticCheck<CR>

" |==== Toggle whitespace visibility with ,s {{{2
nmap <Leader>s :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬


" |==== Easier search/replace {{{2
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s///g<LEFT><LEFT>
vmap S :s///g<LEFT><LEFT>

" |==== Common mistypes and abbreviations {{{2
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa


" |==== Pasting {{{2
" Paste and move to end
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Select what was pasted
noremap gV `[v`]
" Utility ----- {{{1
" |====  Custom fold bind {{{2
function! ToggleFold()
    if &foldlevel < 10
        set foldlevel=99
    else
        set foldlevel=0
    endif
endfunction

noremap zt :call ToggleFold()<CR>

" |====  Custom fold text {{{2
function! CustomFoldText()
     "get first non-blank line
     let fs = v:foldstart
     while getline(fs) =~ '^\s*$' | let fs = nextnonblank(fs + 1)
     endwhile
     if fs > v:foldend
         let line = getline(v:foldstart)
     else
         let line = substitute(getline(fs), '\t', repeat(' ', &tabstop), 'g')
     endif
 
     let w = winwidth(0) - &foldcolumn - (&number ? 6 : 0)
     let foldSize = 1 + v:foldend - v:foldstart
     let foldSizeStr = " [" . foldSize . " lines] "
     let foldLevelStr = repeat("|--", v:foldlevel)
     let lineCount = line("$")
     let expansionString = repeat("⋅", w - strwidth(foldSizeStr.line.foldLevelStr))
     return line . expansionString . foldSizeStr . foldLevelStr
 endfunction
 set foldnestmax=1
 set foldtext=CustomFoldText()


" |====  Nerdtree {{{2
let NERDTreeMinimalUI=1
let NERDTreeChDirMode=2
let NERDTreeRespectWildIgnore=1
let NERDTreeQuitOnOpen=1
let NERDTreeWinSize=35
let NERDTreeDirArrows=1

" Languages ----- {{{1
" |====  Latex / Vimtex {{{2
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
let g:tex_flavor = "latex"
autocmd BufWritePre *.tex :VimtexRefreshFolds

" |====  C++ {{{2
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

" Filetype management {{{1
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.tex set filetype=tex
autocmd FileType c      set foldmethod=syntax
autocmd FileType python set foldmethod=indent
autocmd FileType go     set foldmethod=syntax
autocmd FileType make   set noexpandtab
autocmd FileType rust   set foldmethod=syntax
autocmd FileType vim    set foldmethod=marker

let g:pymode_python = 'python3'

let g:syntastic_python_python_exec = '/usr/local/bin/python3'
