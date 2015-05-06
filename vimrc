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


" Plugins (JuneGunn) ----- {{{1
call plug#begin('~/.vim/plugged')

Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'Shougo/unite-outline'
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimproc.vim'
Plug 'ap/vim-css-color'
Plug 'dahu/vim-fanfingtastic'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go'
Plug 'guns/vim-sexp'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/vim-peekaboo'
Plug 'lervag/vimtex'
Plug 'majutsushi/tagbar'
Plug 'mattn/emmet-vim'
Plug 'nvie/vim-flake8'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/DoxygenToolkit.vim'
Plug 'wlangstroth/vim-racket'
Plug 'wting/rust.vim'

call plug#end()


" Appearance ----- {{{1
" ----- Colourscheme {{{2
set bg=dark
let g:hybrid_use_Xresources = 1
colorscheme base16-monokai
set t_ut=

" ----- Increase/decrease font size {{{2
command! -bar -nargs=0 Bigger  :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)+1','')
command! -bar -nargs=0 Smaller :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)-1','')
noremap \fd        :Smaller<CR>
noremap \fu        :Bigger<CR>

" ----- Relative line numbers {{{2
au BufReadPost * set relativenumber
"}}}
" ----- Nerdtree {{{2
let NERDTreeMinimalUI=1
let NERDTreeChDirMode=2
let NERDTreeRespectWildIgnore=1
let NERDTreeQuitOnOpen=1
let NERDTreeWinSize=35
let NERDTreeDirArrows=1
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
nnoremap <leader>e :NERDTreeToggle<CR>
nnoremap <leader>t :TagbarToggle<CR>
nnoremap <leader>w :w !wc %<CR>
nnoremap <leader>g :Goyo<CR>
nnoremap <leader>v :VimtexTocToggle<CR>
nnoremap <leader>c :SyntasticCheck<CR>


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
" ----- Unite.vim {{{2
nnoremap <leader>f :<C-u>UniteWithProjectDir -start-insert -winheight=7 file_rec/async<cr>
nnoremap <leader>y :<C-u>Unite -buffer-name=yank -winheight=15 history/yank<cr>
nnoremap <leader>b :<C-u>Unite -buffer-name=buffer -quick-match -winheight=7 buffer<cr>
nnoremap <leader>a :<C-u>Unite grep:.<cr>
nnoremap <leader>o :<C-u>Unite -buffer-name=outline -auto-resize outline<CR>

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
function! CustomFoldText()
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
 endfunction
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
let g:syntastic_python_python_exec = '/usr/local/bin/python3'

" ----- Unite {{{2
let g:unite_source_history_yank_enable = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#custom#source('file_rec,file_rec/async', 'ignore_pattern', '\..*/' )

call unite#custom#source('file_rec,file_rec/async', 'ignore_globs',
            \ split(&wildignore,','))

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " Enable navigation with control-j and control-k in insert mode
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
endfunction

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
autocmd FileType rust   set foldmethod=syntax
autocmd FileType vim    set foldmethod=marker
let g:tex_flavor = "latex"








"}}}
" ----- C++ {{{2
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'
