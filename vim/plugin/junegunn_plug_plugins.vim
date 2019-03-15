call plug#begin('~/src/github.com/chrisdavison/dotfiles/vim/plugged')
" programming languages
Plug 'JuliaEditorSupport/julia-vim', { 'for': 'julia' }
Plug 'fatih/vim-go', { 'for': 'go', 'do': ':GoInstallBinaries' }
Plug 'lervag/vimtex', { 'for': ['tex', 'latex'] }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'vim-jp/vim-cpp', { 'for': ['c', 'cpp'] }
Plug 'vim-python/python-syntax', { 'for': 'python' }
Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': ['markdown', 'pandoc'] }
Plug 'vim-pandoc/vim-pandoc', { 'for': ['markdown', 'pandoc'] }
Plug 'elixir-editors/vim-elixir', { 'for': 'elixir' }
" utility
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'Konfekt/FastFold'  " More performant fold refreshing
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'dahu/vim-fanfingtastic'  " Let f/F work across line endings
Plug 'dhruvasagar/vim-table-mode' " Refactoring/formatting tables
Plug 'easymotion/vim-easymotion'  " Easily navigate to any word or char in buffer
Plug 'ervandew/supertab'
Plug 'kana/vim-textobj-user'  " Custom text objects
Plug 'jceb/vim-textobj-uri'   " Text object for link-type stuff
Plug 'jpalardy/vim-slime'     " Send commands to tmux
Plug 'junegunn/fzf.vim'       " FZF for buffer/file etc navigation
Plug 'junegunn/limelight.vim' " De-emphasise paragraphs around your current one
Plug 'junegunn/goyo.vim'      " 'Focus' mode (centered text buffer)
Plug 'romainl/vim-qlist'
Plug 'romainl/vim-qf'
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'   " Comment modification/text objects
Plug 'tpope/vim-fugitive'     " Git integration
Plug 'tpope/vim-sensible'     " Sensible vim default settings
Plug 'tpope/vim-surround'     " 'Surround' text objects e.g. csi(
Plug 'tpope/vim-unimpaired'   " Deal with bracket/surrounding pairs
Plug 'tpope/vim-eunuch'       " More integrated unix commands (mv, rm etc)
Plug 'tpope/vim-vinegar'      " Easily navigate directories
Plug 'wellle/targets.vim'
Plug 'itchyny/lightline.vim'  " More visual statusline
Plug 'junegunn/seoul256.vim'  " Seoul256 theme
Plug 'morhetz/gruvbox'
Plug 'natebosch/vim-lsc'
Plug 'kshenoy/vim-signature'
Plug 'ludovicchabant/vim-gutentags'
Plug 'skywind3000/gutentags_plus'
Plug 'reedes/vim-colors-pencil'
call plug#end()
