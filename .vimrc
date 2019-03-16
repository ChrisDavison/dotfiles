let mapleader=" "
" ChrisDavison's vim config
set runtimepath^=~/src/github.com/chrisdavison/dotfiles/vim
" TODO Language server config for rust currently disabled (in ftplugin/rust)
" TODO add abbrevs for my common languages

" =====[ settings  ]=====
syntax enable
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '↳ '
set cpo+=n
set autoindent
set backspace=indent,eol,start
set complete-=i
set smarttab
set nrformats-=octal
set breakindent
set breakindentopt+=shift:2,sbr
set number relativenumber
set iskeyword=a-z,A-Z,_,.,39  " Used e.g. when searching for tags
set hidden
set ruler
set nospell
set foldenable foldlevelstart=99
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=1
set colorcolumn=0
set hlsearch
set ignorecase
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab
set clipboard=unnamed " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw while executing macros
set scrolloff=1
set sidescroll=1
set sidescrolloff=5
set backup
set backupcopy=yes
set backupdir=~/.temp,.
set directory=~/.temp,.
set wildmenu
set wildmode=list:longest,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif
set splitbelow splitright
set laststatus=2
set statusline=\ (%n)\ %F%=\ %m\ %Y\
set conceallevel=2
set formatoptions+=j  "Delete comment char when joining lines
set history=1000
set tabpagemax=5
set sessionoptions-=options
set viminfo^=!
set t_ut= " Fix issues with background color on some terminals
set fillchars=fold:\ 
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'

" =====[ Conditional settings ]=====
if has('persistent_undo')
    set undodir=~/.undodir
endif

if has('path_extra')
    setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if has('nvim')
    set inccommand=nosplit  " Live-preview of :s commands
endif

if !has('nvim') && &ttimeoutlen == -1
    set ttimeout
    set ttimeoutlen = 100
endif

if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
elseif executable('/usr/bin/env\ zsh')
    set shell=/usr/local/bin/zsh
elseif executable('/usr/bin/env\ bash')
    set shell=/bin/bash
else
    echom "No valid shell!"
endif

" =====[ Personal plugins/config ]=====
" Plugins must be sourced (as they have built in laziness)
" and it doesn't work right with runtime
exec 'source '.globpath(&rtp, '*/3rd_party.vim')
runtime! dotfiles/vim/plugin/*.vim

" =====[ autocommands  ]=====
" file-specific are in DOTFILES/vim/ftplugin/<lang>.vim
augroup vimrc
    autocmd!
    autocmd ColorScheme * hi! link SignColumn LineNr
    autocmd TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd CursorHold * silent! checktime " Check for external changes to files
    autocmd VimResized * wincmd= " equally resize splits on window resize
    autocmd User GoyoEnter Limelight
    autocmd User GoyoLeave Limelight!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
    autocmd BufNewFile *.md exec VimNewMarkdown(expand("<afile>"))
    autocmd BufNewFile *.md call StripTrailingWhitespace()
    autocmd BufNewFile *.py call ReadFileTemplate()
augroup END

" =====[ Config for downloaded plugins ]=====
let g:SuperTabDefaultCompletionType = "context"
let g:fastfold_savehook = 0
let g:cd_schedule_words = [ 'TODO' , 'WAITING', 'DONE', 'CANCELLED' ]
