" vim: set foldmethod=marker foldlevel=1:
" ChrisDavison's Vimrc
let mapleader="\\"
" SETTINGS {{{
set nocompatible " Don't force compability with vi
set autochdir    " cd to the directory of the currently edited file
syntax on
filetype plugin indent on
set encoding=utf-8
scriptencoding utf-8
set showcmd " SHow a currently active command in the bottom line of vim
set wrap lbr
let &showbreak = '└ '
set omnifunc=syntaxcomplete#Complete
set number " Line numbers
set iskeyword=a-z,A-Z,_,.,39
set nohidden
set viminfo='10,<50,s10,h,n~/.viminfo
set nospell
set shell=/bin/zsh
set foldenable
set foldtext=CustomFoldText() " Use a custom fold command below for fold text
set foldlevelstart=10
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬
set autoread " Automatically update buffer if file changed externally
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=2  " Useful for more info on some plugins
set tags=./tags,tags

" --- Search options
set incsearch " Search as you type
set gdefault " By default, replace all matches on a line (i.e. always s///g)
set hlsearch " Highlight search results
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

" --- Various coding preferences
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab " Convert tabs to spaces
set smarttab   " Using <BS> at the start of a line deletes <shiftwidth> spaces
set clipboard=unnamed " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw _while_ exeecuting macros
set laststatus=2  " Always show a status line
set title         " Show filename as window title
set sidescrolloff=15
set sidescroll=1
set autoindent    " New lines match the indent of previous line
set complete-=i
set nrformats-=octal
set ruler       " Show cursor row,column position (if statusline not set)
if v:version > 703 || v:version == 703 && has("patch451")
    set formatoptions+=j  " Remove comment char when joining lines
endif
set history=1000
set tabpagemax=50

" --- Put all temp files in one place
set backup
set backupcopy=yes
set backupdir=~/.backup,.
set directory=~/.temp,.

" --- Wildmenu config
set wildmenu
set wildmode=list:longest,full

"""" Ignore certain files and directories in Wildmenu
set wildignore=*.o,*.obj,*~
set wildignore+=*vim/backups*
set wildignore+=node_modules/**
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif

" By default, split to the right and below, rather than left or up
set splitbelow
set splitright

function! FugitiveStatusCD()
    let fs=fugitive#statusline()
    if fs != ''
        return fs . ","
    else
        return ""
    fi
endfunction

" --- Statusbar
set statusline=%<\ %t\ %=%(%l/%L\|%c%),\ %{exists('g:loaded_fugitive')?FugitiveStatusCD():''}\ %Y\ 

" --- Miscellany
let g:netrw_list_hide = "*.swp,*.swo,*.aux"
if has('persistent_undo')
    set undodir=~/.undodir/
    set undofile
    endif
set t_ut= " Fix issues with background color on some terminals
" }}}
" PLUGIN INSTALL {{{
call plug#begin('~/.vim/plugged')

" Individual languages
Plug 'fatih/vim-go'                               " Syntax: Go
Plug 'pangloss/vim-javascript'                    " Syntax: Javascript
Plug 'plasticboy/vim-markdown'                    " Syntax: Markdown
Plug 'leafgarland/typescript-vim'                 " Syntax: Typescript
Plug 'lervag/vimtex'                              " Syntax: Latex
Plug 'udalov/kotlin-vim'                          " Syntax: Kotlin
Plug 'mxw/vim-jsx'                                " Syntax: JSX
Plug 'wting/rust.vim'                             " Syntax: Rust
Plug 'elixir-editors/vim-elixir'                  " Syntax: Elixir
Plug 'racer-rust/vim-racer'                       " Support for Rust & Racer

" Utility
Plug 'christoomey/vim-tmux-navigator'
Plug 'airblade/vim-gitgutter'                     " Add symbol to gutter to show git changes
Plug 'godlygeek/tabular'                          " Support for formatting tables
Plug 'ekalinin/Dockerfile.vim'                    " Syntax: Docker
Plug 'Konfekt/FastFold'                           " Refreshing folds only on save, or fold-usage
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'bps/vim-textobj-python'                     " Text objects for python
Plug 'dahu/vim-fanfingtastic'                     " Make 'F' work across newlines
Plug 'dhruvasagar/vim-table-mode'                 " Format tables
Plug 'easymotion/vim-easymotion'                  " Easily navigate to any character on screen
Plug 'ervandew/supertab'                          " Use <Tab> for all insertions
Plug 'garbas/vim-snipmate'
Plug 'rbonvall/snipmate-snippets-bib'             " Snippets for bibtex files
Plug 'guns/vim-sexp'
Plug 'honza/vim-snippets'
Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'                          " Distraction free mode
Plug 'junegunn/limelight.vim'                     " Typewriter mode for distraction free
Plug 'kana/vim-textobj-user'                      " Custom text objects ('verbs')
Plug 'kkoenig/wimproved.vim'                      " Better experience on windows (fullscreen)
Plug 'Shougo/echodoc.vim'
Plug 'paulhybryant/vim-textobj-path'              " Text object for paths
Plug 'rking/ag.vim'                               " Support for TheSilverSearcher
Plug 'terryma/vim-expand-region'                  " Keybind to expand the scope of your selection
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-fugitive'                         " Better git integration with vim
Plug 'tpope/vim-obsession'                        " Better session management with vim
Plug 'tpope/vim-sensible'                         " Sensible defaults
Plug 'tpope/vim-sexp-mappings-for-regular-people' " Better Sexp mappings
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'wellle/targets.vim'
Plug 'freitass/todo.txt-vim'
Plug 'gmoe/vim-espresso'

call plug#end()
" }}}
" APPEARANCE {{{
let s:has_256co=$TERM == "xterm-256color" || 
            \ $TERM == "screen-256color" || 
            \ $COLORTERM == "gnome-terminal"
if s:has_256co
   set t_Co=256
else
   colorscheme default
   set t_Co=8
   set t_Sf=[3%dm
   set t_Sb=[4%dm
endif
" Variables for theme switching using my custom plugin daynight.vim
set bg=dark
let g:themeswitch_day='paramount'
let g:themeswitch_night='jellybeans'
execute 'colorscheme ' . g:themeswitch_night
hi! link SignColumn LineNr
hi! link htmlItalic Comment
" }}}
" ABBREVIATIONS {{{
" Command abbreviations
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa
cnoreabbrev Qa qa

" Shortcuts for utility inputs
iabbrev @@ c.jr.davison@gmail.com

" Unicode symbols
iabbrev invq ¿
iabbrev ndash –
iabbrev mdash —
iabbrev rarr →
iabbrev larr ←
iabbrev lrarr ⇔
iabbrev implarr ⇒
iabbrev forall ∀
iabbrev pardif ∂
iabbrev thereexists ∃
iabbrev notexists ∄
iabbrev memberof ∈
iabbrev notmemberof ∉
iabbrev endproof ∎
iabbrev summ ∑
iabbrev prodd ∏
iabbrev isequal ≡
iabbrev logicint ⋂
iabbrev logicand ∧
iabbrev logicor ∨
iabbrev logicnot ¬
iabbrev approxx ≈
iabbrev there4 ∴
" }}}
" KEYBINDS {{{
" Move splits/windows
map <C-w><C-h> <C-w><S-h>
map <C-w><C-j> <C-w><S-j>
map <C-w><C-k> <C-w><S-k>
map <C-w><C-l> <C-w><S-l>

" Move by VISUAL lines
nnoremap  <buffer> <silent> <C-k> gk
nnoremap  <buffer> <silent> <C-j> gj
nnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> <C-$> g$
vmap  <buffer> <silent> k gk
vmap  <buffer> <silent> j gj
vmap  <buffer> <silent> 0 g0
vmap  <buffer> <silent> $ g$

" Select what was pasted
noremap gV `[v`]

" Buffer/File/Function/Outline navigation using FZF
nnoremap <leader>bb :Buffers<Cr>
nnoremap <leader>p :Files<Cr>
nnoremap <leader>ll :Lines<cr>
nnoremap <leader>lb :BLines<cr>
nnoremap <leader>m :Marks<cr>

" Modify/source my VIMRC
nnoremap <leader>ev :e $MYVIMRC<Cr>G
nnoremap <leader>sv :so $MYVIMRC<Cr>

" Backspace goes to `alternate` file
nnoremap <BS> <C-^>

" From/to Neovim terminals
if has('nvim')
    tnoremap <C-h> <C-\><C-n><C-w>h
    tnoremap <C-j> <C-\><C-n><C-w>j
    tnoremap <C-k> <C-\><C-n><C-w>k
    tnoremap <C-l> <C-\><C-n><C-w>l
    nnoremap <C-h> <C-w>h
    nnoremap <C-j> <C-w>j
    nnoremap <C-k> <C-w>k
    nnoremap <C-l> <C-w>l
endif

" Easily search/replace using last search
nmap S :%s///<LEFT>
vnoremap S :s///<LEFT>

" Remove search highlighting
nnoremap <silent> <C-L> :nohlsearch<CR>

function! ToggleConceal()
    if &conceallevel == 2
        set conceallevel=0
    else
        set conceallevel=2
    endif
endfunction
command! ToggleConceal call ToggleConceal()
nnoremap <silent> <C-y> :ToggleConceal<CR>

" Indent/De-dent visual selection
vnoremap < <gv
vnoremap > >gv

nnoremap <leader>t :Tags<CR>
nnoremap <leader>bt :BTags<CR>
nnoremap <leader>lt :LivedownToggle<cr>
nnoremap <Leader>h :set list!<CR>
nnoremap <F11> :Goyo<Cr>
nnoremap nw :set wrap!<CR>
nnoremap <leader>c :ls<Cr>:bd
nnoremap <leader>r :RotateScheduleWord<Cr>
nnoremap <leader>d :ScheduleDone<Cr>

" Fold with space
noremap <space> :normal zA<CR>
" }}}
" PLUGINS / LANGUAGES {{{
augroup vimrc
    autocmd!
    autocmd FileType c set foldmethod=syntax
    autocmd Filetype cpo set foldmethod=syntax
    autocmd Filetype arduino set foldmethod=syntax
    autocmd FileType python  set foldmethod=indent
    autocmd FileType python  set tabstop=4
    autocmd FileType python  set softtabstop=4
    autocmd FileType python  set iskeyword=a-z,A-Z,_
    autocmd FileType go      set foldmethod=syntax
    autocmd Filetype markdown set conceallevel=0
    autocmd Filetype markdown setlocal foldexpr=MarkdownLevel()
    autocmd Filetype markdown setlocal foldmethod=expr
    autocmd Filetype markdown hi Conceal cterm=NONE ctermbg=NONE
    autocmd Filetype markdown hi Conceal guibg=NONE guifg=NONE
    autocmd BufReadPost *.md setlocal foldmethod=expr
    autocmd BufWritePre *.md,*.py :%s/\s\+$//e
    autocmd BufWritePre *.md,*.py :%s///e
    autocmd FileType make    set noexpandtab
    autocmd FileType rust    set foldmethod=syntax
    autocmd FileType rust nmap gd <Plug>(rust-def)
    autocmd FileType rust nmap gs <Plug>(rust-def-split)
    autocmd FileType rust nmap gx <Plug>(rust-def-vertical)
    autocmd FileType rust nmap <leader>gd <Plug>(rust-doc)
    autocmd FileType vim     set foldmethod=marker
    autocmd ColorScheme * hi! link SignColumn LineNr
    autocmd FileType javascript set filetype=javascript.jsx
    autocmd FileType javascript,javascript.jsx set foldmethod=syntax
    autocmd BufNewFile,BufReadPost *.tex set filetype=tex
    autocmd User GoyoEnter Limelight
    autocmd User GoyoLeave Limelight!
    autocmd TextChanged,InsertLeave,FocusLost * silent! wall " Write files on focus lost
    autocmd CursorHold * silent! checktime " Check for external changes to files
    autocmd VimResized * wincmd= " equally resize splits on window resize
augroup END

" PYTHON
let g:pymode_python = 'python3'
let g:slime_target = "tmux"
let g:slime_python_ipython = 1
let g:slime_paste_file=tempname()
" GO
let g:go_fmt_command = "goimports"
" Markdown
let g:vim_markdown_folding_disabled = 0
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_follow_anchor = 1
" LATEX
let g:tex_flavor = "latex"
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
let g:vimtex_indent_enabled=1
let g:vimtex_fold_enabled=1
" TABLES
let g:table_mode_corner="|"
let g:table_mode_corner_corner="|"
let g:table_mode_header_fillchar="-"
" ULTISNIPS
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpForwardTrigger="<c-z>"
let g:UltiSnipsEditSplit="vertical"
" GOYO
let g:goyo_width=100
" EXTRA
let b:javascript_fold=1
let g:SuperTabDefaultCompletionType = "context"
" RUST
let g:racer_cmd="/Users/davison/.cargo/bin/racer"
let g:racer_experimental_completer=1
let g:echodoc_enable_at_startup=1
" GITGUTTER
let g:gitgutter_sign_added = '∙'
let g:gitgutter_sign_modified = '∙'
let g:gitgutter_sign_removed = '∙'
let g:gitgutter_sign_modified_removed = '∙'
" Perl
let perl_fold = 1
" Matching
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif
" }}}
" FZF && Rg/Ag {{{
if executable('rg')
    set grepprg=rg\ --vimgrep
    " let s:find_cmd=
    command! -bang -nargs=* Find call fzf#vim#grep(
    \    'rg --column  --no-heading -F --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
    nnoremap <leader>F :Find<SPACE>
endif
" }}}
" FUNC - folding {{{
function! CustomFoldText()
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . repeat(" ",fillcharcount) . foldedlinecount . ' '
endfunction

" Get the number of # in header to determine foldlevel for markdown
function! MarkdownLevel()
    let h = matchstr(getline(v:lnum), '^#\+')
    if empty(h)
        return "="
    endif
    return ">" . len(h)
endfunction
" }}}
" Miscellaneous functions and commands {{{
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Today exec 'put=strftime(\"%Y-%m-%d\")'
command! TodayNamed exec 'put=strftime(\"%Y-%m-%d - %a\")'
command! TimeNow exec 'put=strftime(\"%Y-%m-%d %H:%M:%S\")'
command! TimeShort exec 'put=strftime(\"**%H:%M**\")'
command! Bd bp|bd # | :echo "Buffer deleted and showing previous"
command! TEOL %s/\s\+$//e | :echo "EOL cleaned"
command! CLEAN retab | TEOL | :echo "Retabbed and EOL cleaned"
" }}}
" Scratch buffers {{{
command! -bar -nargs=? -bang Scratch :silent enew<bang>|set buftype=nofile bufhidden=hide noswapfile buflisted filetype=<args> modifiable
command! -bar -nargs=? -bang SScratch :silent new<bang>|set buftype=nofile bufhidden=hide noswapfile buflisted filetype=<args> modifiable
nnoremap <silent>  == :Scratch<CR>
nnoremap <silent>  =" :Scratch<Bar>put<Bar>1delete _<Bar>filetype detect<CR>
nnoremap <silent>  =* :Scratch<Bar>put *<Bar>1delete _<Bar>filetype detect<CR>
nnoremap <silent>  =p :SScratch<Bar>put *<Bar>1delete _<Bar>filetype detect<CR>
nnoremap           =f :Scratch<Bar>set filetype=
" }}}
" PLATFORM - Windows {{{
if has('win32') || has('win64')
    set shell=cmd.exe
    set shellcmdflag=/c
    set guifont=Fantasque_Sans_Mono:h16
    let gitgutter_enabled=0
    let g:racer_cmd="c:\\Users\\user01\\.cargo\\bin\\racer.exe"
    let g:notedir="e:\\Dropbox\\n\\notes\\"
endif
" }}}
" EXPERIMENTAL {{{
function! GetSyntaxScope()
    let hi="hi<" . synIDattr(synID(line("."),col("."),1),"name") . '>'
    let trans="trans<" . synIDattr(synID(line("."),col("."),0),"name") . ">"
    let lo="lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"
    echo hi . " " . trans . " " . lo
endfunction
command! CurrentSyntax call GetSyntaxScope()
" }}}
