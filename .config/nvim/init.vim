let mapleader=" "

syntax enable
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '>>'
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
set nofoldenable  " OR foldenable foldlevelstart=0
set updatetime=300 " Write a swap file after 1 second
set cmdheight=2
set colorcolumn=0
set hlsearch
set ignorecase smartcase " ignore case unless i specifically mix letter case
set tabstop=4 softtabstop=4 shiftround shiftwidth=4 expandtab
set clipboard=unnamedplus " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw while executing macros

" Some servers have issues with backup files
set nobackup
set nowritebackup

set backupcopy=yes
set backupdir=~/.temp,.
set directory=~/.temp,.
set wildmenu
set wildmode=list:longest,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~
set nojoinspaces   " don't autoinsert two spaces after '.' etc in join
set switchbuf=useopen,usetab
set splitbelow splitright
set laststatus=2
set conceallevel=2
set formatoptions+=j  "Delete comment char when joining lines
set history=1000
set tabpagemax=5
set sessionoptions-=options
set viminfo^=!
set t_ut= " Fix issues with background color on some terminals
set relativenumber
set fillchars=fold:·
let g:netrw_list_hide= '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$'

" suppress 'match x of y', 'only match'... etc
set shortmess+=c

set signcolumn=yes

set path+=**
set statusline=%<\ %n:%f\ %m%r%y%=%-35.(line:\ %l\ of\ %L,\ col:\ %c%V\ (%P)%)

" undo (save undo history across sessions)
set undodir=~/.undodir
set undofile

" shell (specialised per os)
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
elseif executable('/usr/bin/fish')
    set shell=/usr/bin/fish
elseif executable('/usr/bin/zsh')
    set shell=/usr/bin/zsh
elseif executable('/usr/bin/bash')
    set shell=/usr/bin/bash
elseif executable('/bin/bash')
    set shell=/bin/bash
else
    echom "No valid shell!"
endif

" CONDITIONAL SETTINGS / RANDOM STUFF
if has('path_extra')
    setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if has('nvim')
    set inccommand=nosplit  " Live-preview of :s commands
endif

" plugins
"  ____  _    _   _  ____ ___ _   _ ____  
" |  _ \| |  | | | |/ ___|_ _| \ | / ___| 
" | |_) | |  | | | | |  _ | ||  \| \___ \ 
" |  __/| |__| |_| | |_| || || |\  |___) |
" |_|   |_____\___/ \____|___|_| \_|____/ 
call plug#begin('~/.vim/3rd_party')
Plug 'JuliaEditorSupport/julia-vim'
Plug 'fatih/vim-go'
Plug 'lervag/vimtex'
Plug 'rust-lang/rust.vim'
Plug 'vim-jp/vim-cpp'
Plug 'vim-python/python-syntax'
Plug 'plasticboy/vim-markdown'
Plug 'elixir-editors/vim-elixir'
Plug 'cespare/vim-toml'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'dahu/vim-fanfingtastic'  " Let f/F work across line endings
Plug 'easymotion/vim-easymotion'  " Easily navigate to any word or char in buffer
Plug 'kana/vim-textobj-user'  " Custom text objects
Plug 'jceb/vim-textobj-uri'   " Text object for link-type stuff (`go` will open urls)
Plug 'tpope/vim-commentary'   " Comment modification/text objects
Plug 'tpope/vim-surround'     " 'Surround' text objects e.g. csi(
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'      " Easily navigate directories
Plug 'wellle/targets.vim'
Plug 'airblade/vim-gitgutter'
Plug 'ludovicchabant/vim-gutentags'
Plug 'christoomey/vim-tmux-navigator'
Plug 'Shougo/echodoc'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'mbbill/undotree'
Plug 'majutsushi/tagbar'
Plug 'liuchengxu/vim-clap'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'Scuilion/markdown-drawer'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }

" Themes
Plug 'tomasr/molokai'
Plug 'junegunn/seoul256.vim'
Plug 'arzg/vim-corvine'
Plug 'morhetz/gruvbox'
Plug 'sonph/onehalf'
Plug 'jacoborus/tender.vim'
Plug 'jnurmine/Zenburn'
Plug 'AlessandroYorba/Alduin'


call plug#end()


                                                  
" keybinds
"  _  _________   ______ ___ _   _ ____  ____  
" | |/ / ____\ \ / / __ )_ _| \ | |  _ \/ ___| 
" | ' /|  _|  \ V /|  _ \| ||  \| | | | \___ \ 
" | . \| |___  | | | |_) | || |\  | |_| |___) |
" |_|\_\_____| |_| |____/___|_| \_|____/|____/ 
if has('nvim')
    nnoremap <leader>ev :edit ~/.config/nvim/init.vim<CR>
else
    nnoremap <leader>ev :edit ~/.vimrc<CR>
endif

" These versions are for when I don't have fzf and fzf.vim installed
" nnoremap <leader>en :edit ~/src/github.com/chrisdavison/knowledge/**/*
" nnoremap <leader>b :ls<Cr>:b
" nnoremap <leader>s  :ls<CR>:filt  ls<LEFT><LEFT><LEFT>
" nnoremap <leader>p :find

nnoremap <leader>en :Files ~/src/github.com/chrisdavison/knowledge<CR>
nnoremap <leader>es :Files ~/src/github.com/chrisdavison/scripts<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>p :Files<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <silent> <CR> :nohlsearch<CR>

" Use // to search visual selection
vnoremap // y/<c-r>"<CR>

" =====[ Uppercase the current word (from anywhere within the <word>) ]=====
inoremap <C-u>   <esc>mzgUiw`za

nnoremap <silent> Q =ip

" =====[ Generic useful stuff ]=====
nnoremap <BS>   <C-^>
nnoremap S      :%s///<LEFT>
vnoremap S      :s///<LEFT>
vnoremap <      <gv
vnoremap >      >gv
nnoremap j      gj
nnoremap k      gk

nnoremap K :silent! lgrep! "\b<C-R><C-W>\b"<CR>:lw<CR>
nnoremap <leader>g :silent! lgrep! ""<LEFT>

nnoremap <leader>n :Explore ~/Dropbox/notes<CR>

nmap s <Plug>(easymotion-s2)
map <leader>j <Plug>(easymotion-j)
map <leader>k <Plug>(easymotion-k)

nnoremap <leader>md :MarkDrawer<CR>

" <C-C> doesn't trigger InsertLeave autocmd, so rebind to esc
inoremap <c-c> <ESC>

nnoremap <leader>md :MarkDrawer<CR>
let g:markdrawer_toc='full_index'

let g:go_fmt_command="goimports"
let g:go_version_warning=0
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'rust', 'go', 'c', 'cpp']

let g:pymode_python = 'python3'
let g:slime_paste_file=tempname()
let g:slime_python_ipython = 1
let g:slime_target = "tmux"

let g:rustfmt_autosave=1
let g:is_bash=1
let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'

" abbreviations
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
cnoreabbrev Q! q!

iabbrev meanstd μ±σ
iabbrev SALS **See also**:

" custom commands
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Bd bp|bd #
command! Wd write|Bd
command! Scratch edit ~/.scratch | normal G
command! CD exec "cd ".expand("%:h")
command! RMD exec "!rm ".expand("%") | bp | bd #
command! FMT exec "silent!normal mzgg=G`zmzzz"
command! FindWord exec "Rg " . expand("<cword>")

nnoremap <leader>s  :Scratch<CR>

" make nonexistent directories on write
function! MakeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction

" grep / ripgrep
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case

    command! -bang -nargs=* Rg
                \ call fzf#vim#grep(
                \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1, 
                \ fzf#vim#with_preview('right:50%:hidden', '?'),
                \ <bang>0)
endif

" appearance
" when do I need termguicolours? why did I switch it off?
" problem between vim and neovim? terminal and gui? windows vs osx?
set termguicolors
set t_Co=256
set bg=dark
silent! colorscheme corvine
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L


let g:tagbar_type_rust = {
    \ 'ctagstype' : 'rust',
    \ 'kinds' : [
        \'T:types,type definitions',
        \'f:functions,function definitions',
        \'g:enum,enumeration names',
        \'s:structure names',
        \'m:modules,module names',
        \'c:consts,static constants',
        \'t:traits',
        \'i:impls,trait implementations',
    \]
    \}

let g:echodoc#enable_at_startup=1
let g:echodoc#type = 'signature'

let g:vim_markdown_folding_disabled = 1
" Run autocommands at the end of vimrc, to use any previously defined
" functions
highlight nonascii guibg=Red ctermbg=1 term=standout

" coc.nvim...?
" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nmap <leader>rn <Plug>(coc-rename)

xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-i)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

augroup vimrc
    autocmd!
    au BufReadPost * syntax match nonascii "[^\u0000-\u007F£]"
    au ColorScheme * hi! link SignColumn LineNr
    au TextChanged,InsertLeave,FocusLost * silent! wall
    au CursorHold * silent! checktime " Check for external changes to files
    au CursorHold * silent call CocActionAsync('highlight')
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au BufEnter .scratch set filetype=markdown
    au BufEnter *.md set filetype=markdown
    au Filetype arduino set filetype=cpp
    au Filetype make setlocal noexpandtab
    au Filetype markdown setlocal equalprg=pandoc\ --to\ markdown-shortcut_reference_links+pipe_tables-simple_tables-fenced_code_attributes\ --columns=80\ --reference-links\ --reference-location=section\ --wrap=none\ --atx-headers
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex setlocal tw=80
    au Filetype tex setlocal colorcolumn=80
    au Filetype tex setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
    autocmd BufWritePre * call MakeNonExDir()
    au FileType python let b:coc_root_patterns = ['.env', '.git']
augroup END
