" INIT ----- {{{1
"-- Good defaults {{{2
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
set viminfo='10,<50,s10,%,h,n~/.viminfo
set nospell
set relativenumber


"-- Search options {{{2
set incsearch
set hlsearch
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

"-- Various coding preferences {{{2
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

"-- Put all temp files in one place {{{2
set backup
set backupdir=~/.backup,.
set directory=~/.temp,.

"-- Wildmenu config {{{2
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

"-- Allow code folding {{{2
set nofoldenable
set shell=/bin/zsh


