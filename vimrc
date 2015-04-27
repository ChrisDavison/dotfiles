" General {{{
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

" ----- Search options
set incsearch
set hlsearch
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

" ----- Various coding preferences
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
"}}}
" Plugins {{{
" ----- Using JuneGunn's 'plugged'
call plug#begin('~/.vim/plugged')

"Plug 'plasticboy/vim-markdown'
"Plug 'reedes/vim-lexical'
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

" }}}
" Appearance {{{
" ----- Colourscheme
set bg=dark
colorscheme seoul256
set t_ut=

" ----- Increase/decrease font size
command! -bar -nargs=0 Bigger  :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)+1','')
command! -bar -nargs=0 Smaller :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)-1','')
noremap \fd        :Smaller<CR>
noremap \fu        :Bigger<CR>

" ----- Allow code folding
set foldenable
set foldmethod=marker

function! ToggleFold()
    if &foldlevel < 10
        set foldlevel=99
    else
        set foldlevel=0
    endif
endfunction

noremap zt :call ToggleFold()<CR>
" }}}
" Lightline / Light statusbar{{{
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \ 'left':[[ 'mode' ], [ 'fugitive' ]]
      \},
      \ 'component': {
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '>', 'right': '<' }
      \ }
"}}}
" Distraction-free writing {{{
let g:goyo_width=100
let g:goyo_margin_top=1
let g:goyo_margin_bottom=1
autocmd User GoyoEnter Limelight
autocmd User GoyoLeave Limelight! 
"}}}
" Filetype/syntax management{{{
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
" Custom fold text {{{
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
 
     let w = winwidth(0) - &foldcolumn - (&number ? 7 : 0)
     let foldSize = 1 + v:foldend - v:foldstart
     let foldSizeStr = " " . foldSize . " lines "
     let foldLevelStr = repeat("+--", v:foldlevel)
     let lineCount = line("$")
     let foldPercentage = printf("[%.1f", (foldSize*1.0)/lineCount*100) . "%] "
     let expansionString = repeat(".", w - strwidth(foldSizeStr.line.foldLevelStr.foldPercentage))
     return line . expansionString . foldSizeStr . foldPercentage . foldLevelStr
 endf
 set foldnestmax=1
 set foldtext=CustomFoldText()
"}}}
" Key Bindings {{{
" ----- Easier escape and search
map <space> /
imap ii <esc>
nnoremap ; :
nnoremap : :

" ----- Split management
map vv <C-w>v
map vn <C-w>n
map <C-w><C-h> <C-w><S-h>
map <C-w><C-j> <C-w><S-j>
map <C-w><C-k> <C-w><S-k>
map <C-w><C-l> <C-w><S-l>

" ----- Use Ctrl-HJKL to switch windows
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>

" ----- Map to visible rather than literal lines
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$

" ----- For various useful plugins
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


" ----- Jump to specific files
nnoremap  \gc :tabe $MYVIMRC<CR>
nnoremap  \gr :tabe ~/Dropbox/docs/rust.tex<CR>
nnoremap  \gu :tabe ~/Dropbox/docs/uni.tex<CR>
nnoremap  \gn :tabe ~/Dropbox/docs/notes.tex<CR>

" -----Toggle whitespace visibility with ,s
nmap <Leader>s :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬

" ----- Get rid of some 'stupidity' 
noremap K <nop>
nnoremap Q <nop>

" ----- Easier search/replace
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s///g<LEFT><LEFT>
vmap S :s///g<LEFT><LEFT>
"}}}
" CtrlP config {{{
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
let g:ctrlp_working_path_mode = ''
"}}}
" Some common miss-types/abbreviations {{{
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa
"}}}
" Put all temp files in one place {{{
set backup
set backupdir=~/.vim/backup,.
set directory=~/.vim/tmp,.
"}}}
" Allow markdown preview {{{
let g:PreviewCSSPath='http://www.chrisdavison.org/assets/md_preview.css'
let g:PreviewBrowsers='google-chrome,google-chrome-stable,safari,firefox'
"}}}
" Easy alignment mode {{{
" ----- Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)
" ----- Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
"}}}
" Vim Jump-to-File {{{
" DOESN'T WORK---AT ALL
" let g:jumpFiles = ['~/.emacs.d/Chris.org']
" function! JumpToFile()
"     for f in &g:jumpFiles
"         :e f
"     endfor
" endfunction
" }}}
" Relative line numbers {{{
autocmd InsertEnter * :set number
autocmd InsertLeave * :set relativenumber
" }}}
" Refresh tex folds on save {{{
autocmd BufWritePre *.tex :VimtexRefreshFolds
" }}}
" Return to last edit position when opening files  {{{
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%
" }}}
" OTHER plugin configs {{{
let NERDTreeMinimalUI=1
"}}}
" Wildmenu config {{{
set wildmode=list:longest
set wildmenu                
" Ignore some stuff
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
"}}}
" Syntastic {{{
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" }}}
