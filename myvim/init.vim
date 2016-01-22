" Good defaults {{{1
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
set hidden
set nospell

" Search options {{{1
set incsearch
set hlsearch
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

" Various coding preferences {{{1
set tabstop=2
set softtabstop=2
set shiftwidth=2
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

" Put all temp files in one place {{{1
set backup
set backupdir=~/.vim/backup,.
set directory=~/.vim/tmp,.

" Wildmenu config {{{1
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

" Allow code folding {{{1
set foldenable
" Highlight long rows ----- {{{1
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)
