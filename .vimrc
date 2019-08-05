let mapleader=" "
" ChrisDavison's vim config
set runtimepath^=~/code/dotfiles/vim
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
set number 
set iskeyword=a-z,A-Z,_,.,39  " Used e.g. when searching for tags
set hidden
set ruler
set nospell
set foldenable foldlevelstart=99
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=1
set colorcolumn=0
set hlsearch
set ignorecase smartcase
set tabstop=4 softtabstop=4 shiftround shiftwidth=4 expandtab
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
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux
set splitbelow splitright
set laststatus=2
set statusline=%1*[%l:%c]\ %t\ (%{Dir1()})\ %0*%=%1*\ %Y%R\ %0*
hi User1 guifg=white guibg=purple
set conceallevel=2
set formatoptions+=j  "Delete comment char when joining lines
set history=1000
set tabpagemax=5
set sessionoptions-=options
set viminfo^=!
set t_ut= " Fix issues with background color on some terminals
set fillchars=fold:\ 
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'

function! Dir1() " Get only the trailing directory for the statusline
    return fnamemodify(getcwd(), ":t")
endfunction

" =====[ Conditional settings ]=====
if has('persistent_undo')
    set undodir=~/.undodir
endif
set undofile

if has('path_extra')
    setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if has('nvim')
    set inccommand=nosplit  " Live-preview of :s commands
endif

if !has('nvim') && &ttimeoutlen == -1
    set ttimeout
    set ttimeoutlen=100
endif

if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
elseif executable('/usr/local/bin/zsh')
    set shell=/usr/local/bin/zsh
elseif executable('/usr/local/bin/bash')
    set shell=/usr/local/bin/bash
elseif executable('/bin/bash')
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
    autocmd User GoyoEnter Limelight | exec "normal zz" | Typewrite
    autocmd User GoyoLeave Limelight! | Typewrite!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
    autocmd BufNewFile *.md exec VimNewMarkdown(expand("<afile>"))
    autocmd BufNewFile *.py call ReadFileTemplate()
    autocmd BufWritePre *.md call StripTrailingWhitespace()
augroup END

" =====[ Config for downloaded plugins ]=====
let g:SuperTabDefaultCompletionType = "context"

if executable('rg')
    set grepprg=rg\ --vimgrep
endif

if has('gui')
    set gfn=CamingoCode-Regular:h18
endif
