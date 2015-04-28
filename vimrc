" Author: Chris Davison
" Comments: My Vim config.  Not much language-specific stuff in this, as the
" plugin defaults seem to be pretty good in most cases.  For the record, using
" JuneGunn's 'plugged' plugin manager, as I find it's simplistic approach the
" most appealling.  If your fold is set to "marker" for the "vim" filetype,
" then this should fold in a relatively well explained manner, allowing you to
" find and change whatever you need.

" Initial ----- {{{1
" ----- Good defaults {{{2
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

" ----- Search options {{{2
set incsearch
set hlsearch
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

" ----- Various coding preferences {{{2
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

" ----- Put all temp files in one place {{{2
set backup
set backupdir=~/.vim/backup,.
set directory=~/.vim/tmp,.

" ----- Wildmenu config {{{2
set wildmode=list:longest
set wildmenu                
" ----- Wildmenu ignores {{{2
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

" ----- Allow code folding {{{2
set foldenable
set foldmethod=marker


" Plugins ----- {{{1
" ---- Using Junegunn's `plugged' {{{2
call plug#begin('~/.vim/plugged')

Plug 'Blackrush/vim-gocode'
Plug 'Lokaltog/vim-easymotion'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'ap/vim-css-color'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go'
Plug 'greyblake/vim-preview'
Plug 'neilagabriel/vim-geeknote'
Plug 'guns/vim-sexp'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'junegunn/vim-peekaboo'
Plug 'junegunn/vim-easy-align'
Plug 'kien/ctrlp.vim'
Plug 'lervag/vimtex'
Plug 'xolox/vim-colorscheme-switcher'
Plug 'majutsushi/tagbar'
Plug 'mattn/emmet-vim'
Plug 'nelstrom/vim-markdown-folding'
Plug 'reedes/vim-wordy'
Plug 'rizzatti/dash.vim'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'sjl/gundo.vim'
Plug 'tmhedberg/SimpylFold'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-scriptease'
Plug 'xolox/vim-misc'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'wlangstroth/vim-racket'
Plug 'wting/rust.vim'

call plug#end()


" Appearance ----- {{{1
" ----- Colourscheme {{{2
set bg=dark
colorscheme apprentice
set t_ut=

" ----- Increase/decrease font size {{{2
command! -bar -nargs=0 Bigger  :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)+1','')
command! -bar -nargs=0 Smaller :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)-1','')
noremap \fd        :Smaller<CR>
noremap \fu        :Bigger<CR>

" ----- Relative line numbers {{{2
au BufReadPost * set relativenumber
"}}}
" Writing ----- {{{1
" ----- Goyo {{{2
let g:goyo_width=100
let g:goyo_margin_top=1
let g:goyo_margin_bottom=1
autocmd User GoyoEnter Limelight
autocmd User GoyoLeave Limelight! 


" Bindings ----- {{{1
" ----- Easier escape and search {{{2
map <space> /
imap ii <esc>
nnoremap ; :
nnoremap : :

" ----- Split management {{{2
map vv <C-w>v
map vn <C-w>n
map <C-w><C-h> <C-w><S-h>
map <C-w><C-j> <C-w><S-j>
map <C-w><C-k> <C-w><S-k>
map <C-w><C-l> <C-w><S-l>

" ----- Use Ctrl-HJKL to switch windows {{{2
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>

" ----- Map to visible rather than literal lines {{{2
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$

" ----- Visual mode indent {{{2
vnoremap < <gv
vnoremap > >gv


" ----- For various useful plugins {{{2
nnoremap \e :NERDTreeToggle<CR>
nnoremap \t :TagbarToggle<CR>
nnoremap \b :CtrlPBuffer<CR>
nnoremap \p :Preview<CR>
nnoremap \w :w !wc %<CR>
nnoremap \g :Goyo<CR>
nnoremap \u :GundoToggle<CR>
nnoremap \v :VimtexTocToggle<CR>
nnoremap \d :Dash<CR>
nnoremap \c :SyntasticCheck<CR>
nnoremap \bd :Bclose<CR>
nnoremap \cd :cd %:p:h<cr>:pwd<cr>


" ----- Jump to specific files {{{2
nnoremap  \gc :tabe $MYVIMRC<CR>
nnoremap  \gr :tabe ~/Dropbox/docs/rust.tex<CR>
nnoremap  \gu :tabe ~/Dropbox/docs/uni.tex<CR>
nnoremap  \gn :tabe ~/Dropbox/docs/notes.tex<CR>

" ----- Toggle whitespace visibility with ,s {{{2
nmap <Leader>s :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬

" ----- Get rid of some 'stupidity'  {{{2
noremap K <nop>
nnoremap Q <nop>

" ----- Easier search/replace {{{2
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s///g<LEFT><LEFT>
vmap S :s///g<LEFT><LEFT>

" ----- Common mistypes and abbreviations {{{2
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa


" Utility ----- {{{1
" ----- Custom fold bind {{{2
function! ToggleFold()
    if &foldlevel < 10
        set foldlevel=99
    else
        set foldlevel=0
    endif
endfunction

noremap zt :call ToggleFold()<CR>

" ----- Custom fold text {{{2
fu! CustomFoldText()
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
     let expansionString = repeat(".", w - strwidth(foldSizeStr.line.foldLevelStr))
     return line . expansionString . foldSizeStr . foldLevelStr
 endf
 set foldnestmax=1
 set foldtext=CustomFoldText()


" ----- Lightline / Light statusbar {{{2
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {'left':[[ 'mode' ], [ 'fugitive' ]]},
      \ 'component': {
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '>', 'right': '<' }
      \ }

" ----- CtrlP config {{{2
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
let g:ctrlp_working_path_mode = ''

" ----- Minimal NERDtree {{{2
let NERDTreeMinimalUI=1

" ----- Peekaboo {{{2
" Default peekaboo window
let g:peekaboo_window = 'vertical botright 40new'

" Delay opening of peekaboo window (in ms. default: 0)
let g:peekaboo_delay = 750

" Compact display; do not display the names of the register groups
let g:peekaboo_compact = 0

" ----- Syntastic {{{2
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = {
        \ "mode": "active",
        \ "active_filetypes": ["ruby", "php"],
        \ "passive_filetypes": ["latex"] }

" Languages ----- {{{1
" ----- Latex / Vimtex {{{2
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
autocmd BufWritePre *.tex :VimtexRefreshFolds

" ----- Markdown {{{2
let g:PreviewCSSPath='http://www.chrisdavison.org/assets/md_preview.css'
let g:PreviewBrowsers='google-chrome,google-chrome-stable,safari,firefox'

" ----- Filetype management {{{2
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.tex set filetype=tex
autocmd FileType c      set foldmethod=syntax
autocmd FileType go     set foldmethod=syntax
autocmd FileType make   set noexpandtab
autocmd FileType python set foldmethod=syntax
autocmd FileType rust   set foldmethod=syntax
autocmd FileType vim    set foldmethod=marker
let g:tex_flavor = "latex"








"}}}
