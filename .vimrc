" ChrisDavison's vim config
let mapleader=" "
" settings (using tpope/vim-sensible as a base) {{{
" sensible sets...autoindent, better backspace, smarttab, timeout, incsearch,
" ruler, wildmenu, listchars, autoread, history, tabpagemax, viminfo
syntax on
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '↳ '
set cpo+=n
set breakindent
set breakindentopt+=shift:2,sbr
set number relativenumber
set iskeyword=a-z,A-Z,_,.,39  " Used e.g. when searching for tags
set tags=./tags;,tags,.git/tags
set hidden
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
else
    if executable('/usr/local/bin/zsh')
        set shell=/usr/local/bin/zsh
    else
        set shell=/bin/bash
    endif
endif
set nospell
set foldenable foldlevelstart=99
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=1  " Useful for more info on some plugins
set colorcolumn=0 " No color bar (have a toggle command defined below)
set hlsearch " Highlight search results
set ignorecase " Ignore case when searching
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab " Use 4spaces as tabs
set clipboard=unnamed " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw _while_ executing macros
set sidescroll=1
set backup
set backupcopy=yes
set backupdir=~/.temp,.
set directory=~/.temp,.
set wildmode=list:longest,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif
set splitbelow splitright " Split windows down and right by default
set laststatus=2
set statusline=\ (%n)\ %F%=\ %m\ %Y\
set t_ut= " Fix issues with background color on some terminals
set fillchars=fold:\ 
if has('persistent_undo')
    set undodir=~/.undodir/ undofile
endif
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'
set conceallevel=2
" }}}
" plugins {{{
call plug#begin('~/.vim/plugged')
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
call plug#end()
" }}}
" appearance {{{
set t_Co=256
set bg=dark
set termguicolors
silent! colorscheme gruvbox
let g:lightline = { 'colorscheme' : 'seoul256' }
" }}}
" keybinds {{{
" command abbreviatons
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
" indent/de-dent visual selection
vnoremap < <gv
vnoremap > >gv
" Mostly stuff from FZF for navigating buffers
nnoremap <leader>b :Buffers<Cr>
nnoremap <leader>p :Files<Cr>
nnoremap <leader>g :GFiles<Cr>
nnoremap <leader>ll :Lines<cr>
nnoremap <leader>lb :BLines<cr>
nnoremap <leader>m :Marks<cr>
nnoremap <leader>ta :Tags<CR>
nnoremap <leader>tb :BTags<CR>
nnoremap <leader>= gqap
nnoremap <leader>n :NOH<CR>
" easily search/replace using last search
nmap S :%s///<LEFT>
vnoremap S :s///<LEFT>
" Other bindings
nnoremap <leader>ev :silent! e $MYVIMRC<BAR>echo "Editing VIMRC"<CR>
nnoremap <leader>sv :so $MYVIMRC<BAR>echo "Sourced VIMRC"<CR>
nnoremap <leader>ss :mksession! ~/Dropbox/session.vim<BAR>echo "Saved session to dropbox"<CR>
nnoremap <Leader>hh :set list!<BAR>echo "Toggle hidden characters"<CR>
nnoremap nw :set wrap!<BAR>echo "Toggling line wrapping"<CR>
nnoremap <BS> <C-^>
" }}}
" autocommands {{{
augroup vimrc
    autocmd!
    autocmd FileType c,cpp,arduino,go,rust,javascript set foldmethod=syntax
    autocmd FileType python  set foldmethod=indent
    autocmd BufWritePre *.md,*.txt,*.csv %s/\s\+$//e
    autocmd BufNewFile *.md exec VimNewMarkdown(expand("<afile>"))
    autocmd BufWinEnter todo.md highlight TodoDate ctermfg=red
    autocmd BufWinEnter todo.md match TodoDate /\d\d\d\d-\d\d-\d\d/
    autocmd FileType make    set noexpandtab
    autocmd FileType vim     set foldmethod=marker
    autocmd ColorScheme * hi! link SignColumn LineNr
    autocmd TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd CursorHold * silent! checktime " Check for external changes to files
    autocmd VimResized * wincmd= " equally resize splits on window resize
    autocmd User GoyoEnter Limelight
    autocmd User GoyoLeave Limelight!
    autocmd BufWritePre * call MakeNonExDir()
augroup END
" }}}
" plugin/language config {{{
let b:javascript_fold=1
let g:SuperTabDefaultCompletionType = "context"
let g:fastfold_savehook = 0
let g:go_fmt_command = "goimports"
let g:pymode_python = 'python3'
let g:rustfmt_autosave = 1
let g:slime_paste_file=tempname()
let g:slime_python_ipython = 1
let g:slime_target = "tmux"
let g:tex_flavor = "latex"
let g:vimtex_fold_enabled=1
" Fenced code blocks, when using tpope markdown
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'rust', 'go', 'c', 'cpp']
if executable('rls')
    au User lsp_setup call lsp#register_server({
                \ 'name': 'rls',
                \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
                \ 'whitelist': ['rust'],
                \})
endif
let g:pandoc#folding#fdc=0
let g:pandoc#formatting#mode="hA"
let g:pandoc#formatting#textwidth=80
let g:pandoc#spell#enabled=0
let g:pandoc#hypertext#autosave_on_edit_open_link=1
let g:pandoc#hypertext#create_if_no_alternates_exists=1
let g:pandoc#formatting#smart_autoformat_on_cursormoved=1
let g:pandoc#formatting#equalprg="pandoc --to markdown-shortcut_reference_links --columns=80"
let g:pandoc#formatting#extra_equalprg="--reference-links --atx-headers"
" }}}
" custom commands {{{
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
command! ASMR edit ~/Dropbox/asmr.json | normal G
command! Journal edit ~/Dropbox/notes/journal.md | normal G
command! Todos edit ~/Dropbox/notes/todo.md | normal G
command! Dones edit ~/Dropbox/notes/done.md | normal G
command! Projects Explore ~/Dropbox/notes/projects/
command! Scratch edit ~/.scratch | normal G
command! NOH silent! /aksjdkajsd<CR>
" }}}
" TESTING {{{
set inccommand=nosplit  " Live-preview of :s commands
let g:lsc_server_commands = {'python': 'pyls'}
" }}}
" Runtimepath stuff (my config)
set runtimepath+=~/.vim
runtime! make_nonexistent_dir.vim
runtime! logbook.vim
runtime! thesis_notes.vim
runtime! scheduling.vim
runtime! toggle_color_column.vim
runtime! toggle_conceal.vim
runtime! new_markdown_template.vim
runtime! fzf_rg_config.vim
runtime! find_markdown_fold_level.vim

" REMEMBER, autocommand stuff is in runtimepath/ftplugin/<filename>.vim

let g:cd_schedule_words = [ 'TODO' , 'WAITING', 'DONE', 'CANCELLED' ]
nnoremap <leader>r  :RotateScheduleWord<Cr>
nnoremap <silent> <C-y> :call ToggleConceal()<CR>
nnoremap <silent> <leader>tn :ThesisNotes<CR>
