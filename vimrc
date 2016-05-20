let mapleader="\\"

" INIT ----- {{{1
"-- Good defaults {{{2
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
set hidden
set viminfo='10,<50,s10,%,h,n~/.viminfo
set nospell

"-- Search options {{{2
set incsearch
set hlsearch
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

"-- Various coding preferences {{{2
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

"-- Put all temp files in one place {{{2
set backup
set backupdir=~/.backup,.
set directory=~/.temp,.

"-- Wildmenu config {{{2
set wildmode=list:longest
set wildmenu                

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

"-- Allow code folding {{{2
set foldenable
"-- Highlight long rows ----- {{{2
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)
" PLUGINS ----- {{{1
" Using JuneGunn's 'plugged'
call plug#begin('~/.vim/plugged')

"" Languages
Plug 'eagletmt/ghcmod-vim'
Plug 'wlangstroth/vim-racket'
Plug 'wting/rust.vim'
Plug 'pangloss/vim-javascript'
"Plug 'plasticboy/vim-markdown'
Plug 'racer-rust/vim-racer'
Plug 'fatih/vim-go'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-markdownfootnotes'

"" Utility
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimproc.vim'
Plug 'airblade/vim-gitgutter'
Plug 'dahu/vim-fanfingtastic'
Plug 'godlygeek/tabular'
Plug 'guns/vim-sexp'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'h1mesuke/unite-outline'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'kien/ctrlp.vim'
Plug 'lervag/vimtex'
Plug 'majutsushi/tagbar'
Plug 'mattn/emmet-vim'
Plug 'mtth/scratch.vim'
Plug 'nvie/vim-flake8'
Plug 'rking/ag.vim'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-scriptease'
Plug 'lambdalisue/vim-gita'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'vim-scripts/occur.vim'
Plug 'vim-scripts/utl.vim'
Plug 'dhruvasagar/vim-table-mode'
Plug 'pelodelfuego/vim-swoop'

call plug#end()

" Appearance ----- {{{1
set bg=dark
colorscheme Github
set t_ut=

"au BufReadPost * set relativenumber
set gfn=Fira\ Mono:h10

" Highlight long lines
highlight OverLength ctermbg=red ctermfg=white
match OverLength /\%81v.\+/
" Binding ----- {{{1
" Use '//' in visual mode to search for selection
vnoremap // y/<C-R>"<CR>
nnoremap gl vi]y/\[<C-R>"\]<CR>$

" Swap colon and semicolon
nnoremap ; :
nnoremap : :

" Use spacebar for folds
nnoremap <space> za

" EX mode is a pain
map q: :q

" View and switch to buffer
nnoremap gb :ls<CR>:buffer<Space>

" Indent/De-dent visual selection
vnoremap < <gv
vnoremap > >gv


" Split/Window management
" Move windows
map <C-w><C-h> <C-w><S-h>
map <C-w><C-j> <C-w><S-j>
map <C-w><C-k> <C-w><S-k>
map <C-w><C-l> <C-w><S-l>

" Move BETWEEN windows
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>

" Move by VISUAL lines
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$

" Toggle Vimtex Table of Contents
nnoremap <leader>v :VimtexTocToggle<CR>

" Toggle tagbar
nnoremap <leader>t :TagbarToggle<CR>

" Toggle hidden character visibility with
nmap <Leader>s :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬


" Easier search/replace
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s//g<LEFT><LEFT>
vmap S :s//g<LEFT><LEFT>

" Common mistypes and abbreviations
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa


" Selecting and Pasting
" Paste and move to end
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Select what was pasted
noremap gV `[v`]

nnoremap <leader>a ggVG

" Generate a MD preview for the current file
nnoremap mp :!pandoc -s -c ~/.dotfiles/simple-pandoc-css.css % -o ~/.mdpreview.html<CR><CR>

" Custom fold ----- {{{1
function! ToggleFold()
    if &foldlevel < 10
        set foldlevel=99
    else
        set foldlevel=0
    endif
endfunction

noremap zt :call ToggleFold()<CR>


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
     let foldLevelStr = repeat("<", v:foldlevel)
     let lineCount = line("$")
     let expansionString = repeat(" ", w - strwidth(foldSizeStr.line.foldLevelStr))
     return line . expansionString . foldSizeStr . foldLevelStr
 endfunction
 set foldnestmax=99
 set foldtext=CustomFoldText()


" Latex / Vimtex ----- {{{1
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
let g:tex_flavor = "latex"

" C++ ----- {{{1
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

" Python ----- {{{1
let g:pymode_python = 'python3'

let g:syntastic_python_python_exec = '/usr/local/bin/python3'
let g:syntastic_python_checkers = ['flake8']

let g:racer_cmd = "/Users/davison/prog/z__NOT_MINE/racer/target/release/racer"
let $RUST_SRC_PATH="/Users/davison/prog/z__NOT_MINE/rust_1.3_src/src/"

" Filetype Management ----- {{{1
" autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd Filetype markdown setlocal wrap textwidth=80
autocmd Filetype markdown setlocal conceallevel=2
autocmd Filetype markdown hi Conceal cterm=None ctermbg=None

let g:scratch_filetype = 'markdown'

autocmd BufNewFile,BufReadPost *.tex set filetype=tex
autocmd FileType c      set foldmethod=syntax
autocmd FileType python set foldmethod=indent
autocmd FileType python set tabstop=4
autocmd FileType python set softtabstop=4
autocmd FileType go     set nofen
autocmd FileType make   set noexpandtab
autocmd FileType rust   set foldmethod=syntax
autocmd FileType vim    set foldmethod=marker
autocmd BufNewFile,BufReadPost *.es6 set filetype=javascript

let g:pandoc#spell#enabled=0
let g:pandoc#syntax#conceal#urls = 1
let g:pandoc#formatting#mode='ha'
let g:pandoc#formatting#textwidth=80

let b:javascript_fold=1


" Unite ----- {{{1
if executable('ag')
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nocolor --nogroup -S -C4'
  let g:unite_source_grep_recursive_opt=''
endif

" set up coolguy arrow prompt

let g:unite_prompt = '➜ '


" Tagbar Markdown ----- {{{1
" Add support for markdown files in tagbar. 
let g:tagbar_type_markdown = {
    \ 'ctagstype': 'markdown',
    \ 'ctagsbin' : '/Users/davison/prog/z__NOT_MINE/markdown2ctags/markdown2ctags.py',
    \ 'ctagsargs' : '-f - --sort=yes',
    \ 'kinds' : [
        \ 's:sections',
        \ 'i:images'
    \ ],
    \ 'sro' : '|',
    \ 'kind2scope' : {
        \ 's' : 'section',
    \ },
    \ 'sort': 0,
\ }

" Tablemode ----- {{{1
let g:table_mode_corner="|"
let g:table_mode_corner_corner="|"
let g:table_mode_header_fillchar="-"

let g:tex_flavor='latex'

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpForwardTrigger="<c-z>"

let g:UltiSnipsEditSplit="vertical"

" Syntastic ----- {{{1
set statusline+=%#warningmsg#
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
