filetype plugin indent on
syntax enable
let mapleader=" "

" Plugins {{{1
call plug#begin('~/.vim/plugins')

" Utility
Plug 'airblade/vim-gitgutter'
Plug 'chrisdavison/vim-cdroot'
Plug 'chrisdavison/vim-checkmark'
Plug 'chrisdavison/vim-datedfiles'
Plug 'chrisdavison/vim-insertlink'
Plug 'dahu/vim-fanfingtastic'
Plug 'dense-analysis/ale'
Plug 'dhruvasagar/vim-table-mode'
Plug 'easymotion/vim-easymotion'
Plug 'honza/vim-snippets'
Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vim-easy-align'
Plug 'kana/vim-textobj-user'
Plug 'Konfekt/FastFold'
Plug 'kshenoy/vim-signature'
Plug 'ludovicchabant/vim-gutentags'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'romainl/vim-qf'
Plug 'romainl/vim-qlist'
Plug 'simnalamburt/vim-mundo'
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'wellle/targets.vim'
Plug 'dkarter/bullets.vim'

" Language support
Plug 'lervag/vimtex'
Plug 'vim-python/python-syntax'
" Plug 'plasticboy/vim-markdown/'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc'
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'
Plug 'cespare/vim-toml'

" Themes
Plug 'junegunn/seoul256.vim'
Plug 'sainnhe/sonokai'
Plug 'reedes/vim-colors-pencil'
Plug 'sainnhe/edge'

call plug#end()

" }}}1
" settings {{{1
set nocompatible
let &showbreak = '   ┆'
set cpo+=n
set number relativenumber
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
set autochdir
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
let g:netrw_list_hide=netrw_gitignore#Hide() . '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$,.mypy_cache,__pycache__,.*\.aux,.*\.log,.*\.bbl,.*\.blg,.*\.fdb_latexmk,.*\.fls,.*\.log,.*\.out,.*\.toc,.*\.bak'

set smarttab
set nrformats-=octal
set formatoptions+=j
set formatoptions-=a
set signcolumn=yes
set path=.,**
set laststatus=2
set statusline=\ %f\ %4l,%-3c\ %3p%%\ %{fugitive#statusline()}\ %m%r\ %y
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
let g:dark_scheme='edge'
let g:light_scheme='edge'

" Use my colourtoggle functions, defined in ~/.vim/autoload/colourtoggle
call colourtoggle#dark()

command! ColourDark call colourtoggle#dark()
command! ColourToggle call colourtoggle#toggle()
command! ColourLight call colourtoggle#light()
command! ColourTime call colourtoggle#time()

" plugin configuration {{{1
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
let g:slime_default_config = {"socket_name": "default", "target_pane": "{last}"}
let g:slime_dont_ask_default=1
let g:snips_author="C.Davison"
let g:snips_email="c.jr.davison@gmail.com"
let g:datedfile_default_format="%Y%m%d-%A"
let g:datedfile_default_header_format="%Y-%m-%d %A"
let g:table_mode_corner='|'
let g:goyo_width=100
let g:bullets_enabled_file_types = [
    \ 'markdown',
    \ 'text',
    \ 'gitcommit',
    \ 'markdown.pandoc',
    \ 'scratch'
    \]
" keybinds {{{1 
" Format the current paragraph
nnoremap <silent> Q =ip

vnoremap <      <gv
vnoremap >      >gv
" Make j and k work, even on visually-wrapped (not hard-wrapped) lines
nnoremap <expr> j      (v:count == 0? 'gj' : 'j')
nnoremap <expr> k      (v:count == 0? 'gk' : 'k')
nnoremap D      dd
nnoremap Y      y$
" Easier rebind to go to the previously used buffer
nnoremap <BS>   <C-^>
" Tab to toggle fold at point
nnoremap <TAB>  za
" S to quickly jump to a replace using the last searched pattern
nnoremap S :%s///g<LEFT><LEFT>
vnoremap S :s///g<LEFT><LEFT>

" Use esc to go to normal mode in vim's inbuilt terminal
tnoremap <Esc> <C-\><C-n>

" Run 'equalprg' (format) and return to mark
nnoremap <leader>F :normal mzgg=G`zmzzz<CR>

" <C-C> doesn't trigger InsertLeave autocmd, so rebind to esc
inoremap <C-c> <ESC>

" Press 's', followed by a single character, to get a HUD for jumping to a
" word beginning with that character somewhere on screen
nmap s <Plug>(easymotion-s)
let g:EasyMotion_smartcase=1

imap jk <ESC>
imap kj <ESC>

nnoremap <F6> :set paste!<BAR>set paste?<CR>
nnoremap <F9> :Goyo<CR>

" Navigate to stuff in project (files, buffers, or tags)
nnoremap <leader>p :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>g :Files %:p:h<cr>
nnoremap <leader>T :Tags<CR>
nnoremap <leader>t :BTags<CR>

" Navigate to specific files
nnoremap <leader>s :e ~/.scratch<CR>
nnoremap <leader>en :Files ~/code/knowledge/<CR>
nnoremap <leader>ev :e ~/.vimrc<CR>
nnoremap <leader>j :NewJournal<CR>
nnoremap <leader>l :NewLogbook<CR>
nnoremap <leader>c :NewCalendar<CR>



" Navigate :arglist
nnoremap <right> :next<CR>
nnoremap <left> :prev<CR>

nnoremap <leader>z :norm `z<CR>

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

" Use tab to start snippets, c-j/k to navigate the $'s
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-k>"
let g:UltiSnipsEditSplit="horizontal"

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
            \ 'ctrl-v': 'vsplit',
            \ 'ctrl-f': 'InsertFilename',
            \ 'ctrl-i': 'InsertLinkToNote',
            \ 'ctrl-l': 'InsertLinkToNoteBelow'}

command! -complete=file -nargs=1 InsertFilename call append(line("."), fnamemodify(<q-args>, ":~:."))


" abbreviations {{{1
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() =~# '^grep')  ? 'silent grep'  : 'grep'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() =~# '^lgrep') ? 'silent lgrep' : 'lgrep'
cnoreabbrev W w
cnoreabbrev Wq wq
cnoreabbrev Qa qa
cnoreabbrev QA qa
cnoreabbrev E e
cnoreabbrev Q! q!
cnoreabbrev BD bp<bar>bd #
cnoreabbrev Bd bd
iabbrev <expr> DATE strftime("%Y-%m-%d")
iabbrev <expr> DATEB strftime("**%Y-%m-%d**")
iabbrev <expr> TIME strftime("%H:%M:%S")
iabbrev <expr> TS strftime("«%H:%M»")
iabbrev <expr> DATEN strftime("%Y-%m-%d %A")

" commands & functions {{{1
command! MakeTags !ctags -R .
command! ShaID exec 'r!shaid ' . expand('%:p')
command! Goz exec 'norm `z'

function! s:goyo_enter()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status off
    " silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  endif
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
  " ...
endfunction

function! s:goyo_leave()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status on
    " silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  endif
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
  " ...
endfunction

function! s:prompt_and_tidy_filename(dir, split, ...)
    let projectname=input('Title: ')
    let project_nospace=substitute(l:projectname, ' ', '-', 'g')
    let filepath=a:dir . l:project_nospace . '.md'
    let tags=a:000
    if a:split
        split
    endif
    exec ':edit ' . l:filepath
    call append(0, '# ' . <sid>titlecase(l:projectname))
    if !empty(l:tags)
        let tags=map(copy(l:tags), '"@" . v:val')
        call append(1, ['', join(l:tags, " ")])
    endif
endfunction


function! s:titlecase(str)
    let words=split(a:str, '\W\+')
    let titled=map(l:words, {_, word -> toupper(word[0]) . word[1:]})
    return join(l:titled, ' ')
endfunction

function! s:last_file_in_dir(dir)
    let files=glob(fnamemodify(a:dir, ':p') . "*", 0, 1)
    return l:files[len(l:files)-1]
endfunction

command! LastJournal :exec "edit " . <sid>last_file_in_dir("~/code/knowledge/journal")
command! LastLogbook :exec "edit " . <sid>last_file_in_dir(strftime("~/code/logbook/%Y"))
command! NewJournal :DatedFile ~/code/knowledge/journal
command! NewLogbook :DatedFileWithFmt ~/code/logbook %Y/%Y%m%d-%A
command! NewCalendar :DatedFileWithFmt ~/code/knowledge/calendar %Y-%m--%B
" autocommands {{{1
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
    " au BufEnter *.rs,*.py,*.md Root
    au VimLeave * call sessions#save_last()
    au User CocJumpPlaceholder call CocActionSync('showSignatureHelp')
    " au InsertEnter * set norelativenumber
    " au InsertLeave * set relativenumber
    au BufEnter *.md let b:coc_suggest_disable=1
    au User GoyoEnter nested call <sid>goyo_enter()
    au User GoyoLeave nested call <sid>goyo_leave()
    au BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
augroup END
