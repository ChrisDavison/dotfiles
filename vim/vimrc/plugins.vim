call plug#begin('~/.vim/plugged')

" Languages {{{
Plug 'fatih/vim-go'
Plug 'pangloss/vim-javascript'
Plug 'racer-rust/vim-racer'
Plug 'plasticboy/vim-markdown'
" }}}

" Utility {{{
Plug 'Rykka/riv.vim'
Plug 'shime/vim-livedown'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'Konfekt/FastFold'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'dahu/vim-fanfingtastic'
Plug 'easymotion/vim-easymotion'
Plug 'ervandew/supertab'
Plug 'garbas/vim-snipmate'
Plug 'godlygeek/tabular'
Plug 'guns/vim-sexp'
Plug 'honza/vim-snippets'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'kien/ctrlp.vim'
Plug 'lervag/vimtex'
Plug 'majutsushi/tagbar'
Plug 'nvie/vim-flake8'
Plug 'rking/ag.vim'
Plug 'scrooloose/syntastic'
Plug 'tacahiroy/ctrlp-funky'
Plug 'terryma/vim-expand-region'
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-scripts/a.vim'
Plug 'vim-scripts/utl.vim'
"}}}

" Plugins I'm currently *not* using {{{
" Distraction-free writing
if 1
    Plug 'junegunn/goyo.vim'
    Plug 'junegunn/limelight.vim'
endif

" Lispy stuff
if 0
    Plug 'jpalardy/vim-slime'
    Plug 'tpope/vim-sexp-mappings-for-regular-people'
endif

" Utility
if 0
    Plug 'mattn/emmet-vim'
    Plug 'dhruvasagar/vim-table-mode'
endif
" }}}

call plug#end()



