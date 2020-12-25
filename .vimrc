filetype plugin indent on
syntax enable
let mapleader=" "

" Plugins {{{1
call plug#begin('~/.vim/plugins')

" Plugins - Utility {{{2
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
  
Plug 'Konfekt/FastFold'
Plug 'preservim/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'wellle/targets.vim'
Plug 'mbbill/undotree'
Plug 'chrisdavison/vim-cdroot'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
Plug 'easymotion/vim-easymotion'
Plug 'dahu/vim-fanfingtastic'
Plug 'airblade/vim-gitgutter'
Plug 'ludovicchabant/vim-gutentags'
Plug 'romainl/vim-qf'
Plug 'romainl/vim-qlist'
Plug 'tpope/vim-repeat'
Plug 'jpalardy/vim-slime'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'kana/vim-textobj-user'
" Plug 'christoomey/vim-tmux-navigator'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'
Plug 'kshenoy/vim-signature'
Plug 'jiangmiao/auto-pairs'
Plug 'dense-analysis/ale'
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
Plug 'freitass/todo.txt-vim'

" Plugins - Language support {{{2
Plug 'lervag/vimtex'
Plug 'vim-python/python-syntax'
Plug 'plasticboy/vim-markdown/'
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'

" Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neoclide/coc.nvim'

"}}}
" Plugins - Themes {{{2
Plug 'arzg/vim-corvine'
Plug 'junegunn/seoul256.vim'
Plug 'owickstrom/vim-colors-paramount'
Plug 'ayu-theme/ayu-vim'
Plug 'endel/vim-github-colorscheme'
Plug 'jonathanfilip/vim-lucius'
Plug 'tomasr/molokai'
Plug 'arcticicestudio/nord-vim'
Plug 'sainnhe/sonokai'
call plug#end()

" }}}1
" settings {{{1
set nocompatible
let &showbreak = '   ┆'
set cpo+=n
set number 
set wrap lbr
set autoindent
set breakindent
set breakindentopt=shift:4,sbr
set backspace=indent,eol,start
set iskeyword=a-z,A-Z,_,48-57  " Used e.g. when searching for tags
setglobal tags-=./tags tags-=./tags; tags^=./tags;
set incsearch
set updatetime=300 " Write a swap file after 1 second
set autoread
set tabstop=4 softtabstop=4 shiftround shiftwidth=4 expandtab
set clipboard+=unnamedplus " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw while executing macros
set foldlevelstart=99
set noautochdir
set cursorline
set guioptions-=m
set guioptions-=T
if has('gui')
    set gfn=Rec\ Mono\ Casual\ 12
endif
set hlsearch
" set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set listchars=tab:>-
set listchars+=trail:⋄
set cmdheight=2
set shortmess+=c

set scrolloff=1
set sidescrolloff=5
set display+=lastline

" Some servers have issues with backup files
set nobackup nowritebackup
set history=1000
set tabpagemax=50
if !empty(&viminfo)
    set viminfo^=!
endif
set sessionoptions-=options
set viewoptions-=options

set directory=~/.vim/swapfiles//,.
set directory=~/.vim/backups//,.
set ignorecase smartcase " ignore case unless i specifically mix letter case
set wildmenu
set wildmode=longest:list,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~,*tags*
set wildignore+=*.swp,*.so,*.fls,*.log,*.out,*.toc,*.xdv,*.bbl,*.blg,*.fdb_latexmk
set wildignorecase
set nojoinspaces   " don't autoinsert two spaces after '.' etc in join
set switchbuf=useopen,usetab
set splitbelow splitright
set showmode
let g:netrw_list_hide=netrw_gitignore#Hide() . '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$,.mypy_cache,__pycache__,.*\.aux,.*\.log,.*\.bbl,.*\.blg,.*\.fdb_latexmk,.*\.fls,.*\.log,.*\.out,.*\.toc'

set smarttab
set nrformats-=octal
set formatoptions+=j
set formatoptions-=a
set signcolumn=yes
set path=.,**
set laststatus=2
set statusline=\ (%n)\ %t:%l:%c\ %{fugitive#statusline()}\ %m%r\ %y
set ruler
set encoding=utf-8

if !exists('g:loaded_matchit')
    runtime! macros/matchit.vim
endif

set undodir=~/.undodir
set undofile
set complete-=i
set completeopt=menu,menuone,preview

set shell=/usr/bin/zsh
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
endif

if has('nvim')
    set inccommand=nosplit  " Live-preview of :s commands
endif

let g:netrw_browsex_viewer="firefox --new-tab"
" appearance {{{1
set termguicolors
set t_ut= " Fix issues with background color on some terminals
set t_Co=16
if !has('gui_running')
    set t_Co=256
endif
let g:rehash256 = 1
set bg=dark
let g:dark_scheme='sonokai'
let g:light_scheme='github'
function s:colour_time()
    if strftime("%H") >= 21 || strftime("%H") < 8
        call s:colour_dark()
    else
        call s:colour_light()
    end
endfunction

function s:colour_toggle()
    if &bg == "dark"
        call s:colour_light()
    else
        call s:colour_dark()
    endif
endfunction

function s:colour_dark()
    " colorscheme corvine
    exec "colorscheme " . g:dark_scheme
    set bg=dark
endfunction

function s:colour_light()
    " colorscheme corvine_light
    exec "colorscheme " . g:light_scheme
    set bg=light
endfunction

call s:colour_dark()

command! ColourDark call s:colour_dark()
command! ColourToggle call s:colour_toggle()
command! ColourLight call s:colour_light()
command! ColourTime call s:colour_time()

" Rather than modifying 'paramount' directly,
" Just link html (markdown) headers to 'Question' to get
" a pinkish header
if g:colors_name == 'paramount'
    hi! link htmlH1      Question
    hi! link htmlH2      Question
    hi! link htmlH3      Question
    hi! link htmlH4      Question
    hi! link htmlH5      Question
    hi! link htmlH6      Question
endif
" plugin & programming language config {{{1
let g:is_bash=1
let g:fzf_layout = {'down': '~40%'}
let g:fzf_preview_window=''
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case\ -g\ '!tags'
endif
let g:non_git_roots=['~/Dropbox/notes', '/mnt/e/Dropbox/notes']
let g:gutentags_project_root = ['tags']
let g:gutentags_define_advanced_commands=1
let g:go_fmt_command="goimports"
let g:go_fmt_autosave=1
let g:go_version_warning=0
let g:pymode_python = 'python3'
let g:slime_python_ipython = 1
let g:rustfmt_autosave=1
let g:vimtex_format_enabled=1
let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'
let g:slime_target='tmux'

" keybinds {{{1 
nnoremap <silent> Q =ip
vnoremap <      <gv
vnoremap >      >gv
nnoremap <expr> j      (v:count == 0? 'gj' : 'j')
nnoremap <expr> k      (v:count == 0? 'gk' : 'k')
nnoremap D      dd
nnoremap Y      y$
nnoremap <BS>   <C-^>
nnoremap <TAB>  za
tnoremap <Esc> <C-\><C-n>
nnoremap <silent> <expr> <c-\> &colorcolumn == 0 ? ":set colorcolumn=81<cr>" : ":set colorcolumn=0<cr>"
" Run 'equalprg' (format) and return to mark
nnoremap <leader>F :normal mzgg=G`zmzzz<CR>
" <C-C> doesn't trigger InsertLeave autocmd, so rebind to esc
inoremap <C-c> <ESC>
nnoremap <leader>s :e ~/.scratch<CR>
nnoremap <leader>S :e ~/.scratch<BAR>normal ggdG<CR>

nnoremap S :%s///g<LEFT><LEFT>
nmap s <Plug>(easymotion-s)
nmap s <Plug>(easymotion-s2)
let g:EasyMotion_smartcase=1

imap jk <ESC>
imap kj <ESC>
" Keybinds for common commands
nnoremap <leader>p :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>g :Files %:p:h<cr>
nnoremap <leader>T :Tags<CR>
nnoremap <leader>t :BTags<CR>
nnoremap <F2> :NERDTreeToggle<CR>
nnoremap <F6> :set paste!<BAR>set paste?<CR>
nnoremap <leader>i :CocList outline<CR>
nnoremap <leader>j :exec "e ~/code/knowledge/journal/".strftime("%Y%m%d-%A.md")<CR>
nnoremap <leader>l :exec "e ~/code/knowledge/logbook/".strftime("%Y%m%d-%A.md")<CR>
nnoremap <leader><leader> :Files ~/code/knowledge/<CR>

" coc.nvim
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> R :call CocAction('doHover')<CR>
nnoremap <silent> K :call <SID>show_documentation()<CR>
nnoremap <leader>o :CocList symbols<CR>
nnoremap <leader>i :CocList outline<CR>

xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-k>"
let g:UltiSnipsEditSplit="vertical"

function! s:show_documentation()
    if(index(['vim', 'help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction

let g:fzf_action = {
        \ 'ctrl-t': 'tab split',
        \ 'ctrl-x': 'split',
        \ 'ctrl-v': 'vsplit' }
" abbreviations {{{1
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() =~# '^grep')  ? 'silent grep'  : 'grep'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() =~# '^lgrep') ? 'silent lgrep' : 'lgrep'
cnoreabbrev W w
cnoreabbrev Wq wq
cnoreabbrev Qa qa
cnoreabbrev E e
cnoreabbrev Q! q!
cnoreabbrev BD bp<bar>bd #
cnoreabbrev Bd bd
iabbrev <expr> DATE strftime("%Y-%m-%d")
iabbrev <expr> DATEB strftime("**%Y-%m-%d**")
iabbrev <expr> TIME strftime("%H:%M:%S")
iabbrev <expr> DATEN strftime("%Y-%m-%d %A")
" commands {{{1
command! MakeTags !ctags -R .
" autocommands {{{1
function! s:save_last_session()
    exec "!rm ~/.lastsession.vim"
    mks ~/.lastsession.vim
endfunction

augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au Filetype make set noexpandtab
    au Filetype text set formatoptions-=a
    au Filetype vim set foldmethod=marker
    au Filetype zsh,bash,sh set foldmethod=marker
    au Filetype go set foldmethod=syntax
    au Filetype rust set foldmethod=syntax
    au Filetype python set foldmethod=indent formatoptions-=a
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex set foldmethod=expr
                \ foldexpr=vimtex#fold#level(v:lnum)
                \ foldtext=vimtex#fold#text()
                \ fillchars=fold:\  
                \ formatoptions-=a
    au BufEnter .scratch setlocal filetype=markdown
    " Don't use autochdir when using 'Root'
    au BufEnter * Root
    au VimLeave * call s:save_last_session()
    au User CocJumpPlaceholder call CocActionSync('showSignatureHelp')
augroup END

let s:clip = '/mnt/c/Windows/System32/clip.exe' 
if executable(s:clip)
    augroup WSLYank
        autocmd!
        autocmd TextYankPost * call system('echo '.shellescape(join(v:event.regcontents, "\<CR>")).' | '.s:clip)
    augroup END
end

