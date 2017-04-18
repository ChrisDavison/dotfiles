" INIT ----- {{{1
"-- Good defaults {{{2
set nocompatible
syntax on
filetype plugin indent on
set encoding=utf-8
scriptencoding utf-8
set showcmd
set wrap lbr
set showbreak=⇇
set omnifunc=syntaxcomplete#Complete
set number
set iskeyword=a-z,A-Z,_,.,39
set hidden
set viminfo='10,<50,s10,%,h,n~/.viminfo
set nospell
set relativenumber
set shell=/bin/zsh
set nofoldenable " Don't fold by default

"-- Search options {{{2
set incsearch " Search as you type
set gdefault " By default, replace all matches on a line (i.e. always s///g)
set hlsearch " Highlight search results
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

"-- Various coding preferences {{{2
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
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
set wildmenu
set wildmode=list:longest

"""" Ignore certain files and directories in Wildmenu
set wildignore=*.o,*.obj,*~ 
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif

