let mapleader=" "

syntax enable
filetype plugin indent on

" ------------------------------------------------------------------------------
" plugins, managed with github.com/junegunn/plug.vim
" ------------------------------------------------------------------------------
call plug#begin('~/.vim/3rd_party')
Plug 'fatih/vim-go'
Plug 'lervag/vimtex'
Plug 'rust-lang/rust.vim'
Plug 'vim-jp/vim-cpp'
Plug 'vim-python/python-syntax'
Plug 'plasticboy/vim-markdown'
Plug 'cespare/vim-toml'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
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
Plug 'majutsushi/tagbar'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'Scuilion/markdown-drawer'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
Plug 'romainl/vim-qlist'
Plug 'junegunn/rainbow_parentheses.vim'

" Themes
Plug 'tomasr/molokai'
Plug 'junegunn/seoul256.vim'
Plug 'arzg/vim-corvine'

call plug#end()

" ------------------------------------------------------------------------------
" settings
" ------------------------------------------------------------------------------
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
set wildignorecase
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
set relativenumber
set fillchars=fold:·
let g:netrw_list_hide= '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$'

" suppress 'match x of y', 'only match'... etc
set shortmess+=c

set signcolumn=yes

set path+=**
set statusline=%<\ %n:%f\ %m%r%y%=%(%P\ of\ %LL\ -\ %l,%c\ %)

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

" ------------------------------------------------------------------------------
" appearance
" ------------------------------------------------------------------------------
" when do I need termguicolours? why did I switch it off?
" problem between vim and neovim? terminal and gui? windows vs osx?
set termguicolors
set t_ut= " Fix issues with background color on some terminals
set t_Co=256
set bg=dark
silent! colorscheme seoul256
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L


" ------------------------------------------------------------------------------
" settings for plugins
" ------------------------------------------------------------------------------
let g:markdrawer_toc='full_index'
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'rust', 'go', 'c', 'cpp']
let g:go_fmt_command="goimports"
let g:go_version_warning=0
let g:pymode_python = 'python3'
let g:rustfmt_autosave=1
let g:is_bash=1
let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'
let g:echodoc#enable_at_startup=1
let g:echodoc#type = 'signature'
let g:vim_markdown_folding_disabled = 1
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
let g:coc_snippet_next = '<c-j>'
let g:coc_snippet_prev = '<c-k>'


" ------------------------------------------------------------------------------
"  keybinds
" ------------------------------------------------------------------------------
"  keybinds for builtin functionality
if filereadable(expand('~/.config/nvim/init.vim'))
    nnoremap <leader>ev :edit ~/.config/nvim/init.vim<CR>
elseif filereadable(expand('~/.vimrc'))
    nnoremap <leader>ev :edit ~/.vimrc<CR>
endif

" Use // to search visual selection
vnoremap // y/<c-r>"<CR>

nnoremap <silent> Q =ip
nnoremap <BS>   <C-^>
nnoremap S      :%s///<LEFT>
vnoremap S      :s///<LEFT>
vnoremap <      <gv
vnoremap >      >gv
nnoremap j      gj
nnoremap k      gk
nnoremap Y y$
nnoremap <silent> <CR> :nohlsearch<CR>

" Close quickfix or location window
nnoremap <leader>c :cclose<bar>lclose<CR>

" keybinds for installed plugins
nnoremap <leader>en :Files! ~/src/github.com/ChrisDavison/knowledge<CR>
nnoremap <leader>es :Files! ~/src/github.com/ChrisDavison/scripts<CR>
nnoremap <leader>el :NERDTree ~/src/github.com/ChrisDavison/logbook/2019<CR>
nnoremap <leader>p :Files!<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>n :Files! ~/Dropbox/notes/<CR>

nnoremap <C-n> :NERDTreeVCS<CR>


nmap s <Plug>(easymotion-s2)
map <leader>j <Plug>(easymotion-j)
map <leader>k <Plug>(easymotion-k)

" Readline-style keybinds in the command line
cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <expr> <C-D> getcmdpos()>strlen(getcmdline())?"\<Lt>C-D>":"\<Lt>Del>"
cnoremap <expr> <C-F> getcmdpos()>strlen(getcmdline())?&cedit:"\<Lt>Right>"
cnoremap        <M-b> <S-Left>
cnoremap        <M-f> <S-Right>
silent! exe "set <S-Left>=\<Esc>b"
silent! exe "set <S-Right>=\<Esc>f"

" <C-C> doesn't trigger InsertLeave autocmd, so rebind to esc
inoremap <c-c> <ESC>

nnoremap <leader>md :MarkDrawer<CR>
nnoremap <leader>t :TagbarToggle<CR>


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

" ------------------------------------------------------------------------------
" :MakeNonExistentDir | try to make all parent directories of a new buffer
" ------------------------------------------------------------------------------
" make nonexistent directories on write
function! s:makeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction
command! MakeNonExistentDir call s:makeNonExDir()

" grep / ripgrep
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
    command! -bang -nargs=* Rg
        \ call fzf#vim#grep(
        \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
        \ fzf#vim#with_preview('right:50%:hidden', '?'),
        \ <bang>0)
endif

" ------------------------------------------------------------------------------
" highlight non-ascii characters in current buffer
" ------------------------------------------------------------------------------
highlight nonascii guibg=Red ctermbg=1 term=standout

" ------------------------------------------------------------------------------
" keybinds for Coc.nvim, the completion engine
" ------------------------------------------------------------------------------
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

imap <C-j> <Plug>(coc-snippets-expand-jump)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" ----------------------------------------------------------------------------
" EX | chmod +x
" ----------------------------------------------------------------------------
command! EX if !empty(expand('%'))
         \|   write
         \|   call system('chmod +x '.expand('%'))
         \|   silent e
         \| else
         \|   echohl WarningMsg
         \|   echo 'Save the file first'
         \|   echohl None
         \| endif

" ---------------------------------------------------------
" :Root | Change dir to the root of the Git repository
" ---------------------------------------------------------
function! s:root()
  let root = systemlist('git rev-parse --show-toplevel')[0]
  if v:shell_error
    echo 'Not in git repo'
  else
    execute 'lcd' root
    echo 'Changed directory to: '.root
  endif
endfunction
command! Root call s:root()


" ------------------------------------------------------------------------------ 
" abbreviations
" ------------------------------------------------------------------------------ 
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
cnoreabbrev Q! q!

iabbrev meanstd μ±σ
iabbrev SALS **See also**:

" ------------------------------------------------------------------------------
" autocommands
" ------------------------------------------------------------------------------
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
    au Filetype markdown setlocal equalprg=pandoc\ --to\ markdown-shortcut_reference_links+pipe_tables-simple_tables-fenced_code_attributes\ --columns=80\ --reference-links\ --reference-location=section\ --wrap=auto\ --atx-headers
    au Filetype markdown nnoremap <buffer> <leader>t :MarkDrawer<CR>
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex setlocal tw=80
    au Filetype tex setlocal colorcolumn=80
    au Filetype tex setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
    autocmd BufWritePre * call s:makeNonExDir()
    au FileType python let b:coc_root_patterns = ['.env', '.git']
    au FileType markdown map <Bar> vip :EasyAlign*<Bar><Enter>
    au FileType markdown set spell spelllang=en_gb
augroup END
