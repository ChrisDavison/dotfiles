call plug#begin('~/code/dotfiles/vim/3rd_party')
" programming languages
Plug 'JuliaEditorSupport/julia-vim'
Plug 'fatih/vim-go'
Plug 'lervag/vimtex'
Plug 'rust-lang/rust.vim'
Plug 'vim-jp/vim-cpp'
Plug 'vim-python/python-syntax'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc'
Plug 'elixir-editors/vim-elixir'
" utility
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Konfekt/FastFold'  " More performant fold refreshing
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'dahu/vim-fanfingtastic'  " Let f/F work across line endings
Plug 'easymotion/vim-easymotion'  " Easily navigate to any word or char in buffer
Plug 'ervandew/supertab'
Plug 'kana/vim-textobj-user'  " Custom text objects
Plug 'jceb/vim-textobj-uri'   " Text object for link-type stuff
Plug 'jpalardy/vim-slime'     " Send commands to tmux
Plug 'junegunn/limelight.vim' " De-emphasise paragraphs around your current one
Plug 'junegunn/goyo.vim'      " 'Focus' mode (centered text buffer)
Plug 'romainl/vim-qlist'
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'   " Comment modification/text objects
Plug 'tpope/vim-fugitive'     " Git integration
Plug 'tpope/vim-surround'     " 'Surround' text objects e.g. csi(
Plug 'tpope/vim-unimpaired'   " Deal with bracket/surrounding pairs
Plug 'tpope/vim-eunuch'       " More integrated unix commands (mv, rm etc)
Plug 'tpope/vim-vinegar'      " Easily navigate directories
Plug 'wellle/targets.vim'
Plug 'junegunn/seoul256.vim'  " Seoul256 theme
Plug 'natebosch/vim-lsc'
Plug 'kshenoy/vim-signature'
Plug 'ludovicchabant/vim-gutentags'
Plug 'skywind3000/gutentags_plus'
Plug 'tomasr/molokai'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
call plug#end()
