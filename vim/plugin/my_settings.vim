" settings (using tpope/vim-sensible as a base)
" sensible sets...autoindent, better backspace, smarttab, timeout, incsearch,
" ruler, wildmenu, listchars, autoread, history, tabpagemax, viminfo
syntax on
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '↳ '
set cpo+=n
set breakindent
set breakindentopt+=shift:2,sbr
set number relativenumber
set iskeyword=a-z,A-Z,_,.,39  " Used e.g. when searching for tags
set tags=./tags;,tags,.git/tags
set hidden
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
else
    if executable('/usr/local/bin/zsh')
        set shell=/usr/local/bin/zsh
    else
        set shell=/bin/bash
    endif
endif
set inccommand=nosplit  " Live-preview of :s commands
set nospell
set foldenable foldlevelstart=99
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=1  " Useful for more info on some plugins
set colorcolumn=0 " No color bar (have a toggle command defined below)
set hlsearch " Highlight search results
set ignorecase " Ignore case when searching
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab " Use 4spaces as tabs
set clipboard=unnamed " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw _while_ executing macros
set sidescroll=1
set backup
set backupcopy=yes
set backupdir=~/.temp,.
set directory=~/.temp,.
set wildmode=list:longest,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif
set splitbelow splitright " Split windows down and right by default
set laststatus=2
set statusline=\ (%n)\ %F%=\ %m\ %Y\
set t_ut= " Fix issues with background color on some terminals
set fillchars=fold:\ 
if has('persistent_undo')
    set undodir=~/.undodir
endif
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'
set conceallevel=2
