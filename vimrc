" vim: set foldmethod=marker foldlevel=0:
" ChrisDavison's VIMRC {{{ 
" ============================================================================
set nocompatible
syntax on
filetype plugin indent on
let mapleader="\\"
set autochdir
let s:darwin = has('mac')
" }}}
" ============================================================================
" SETTINGS {{{ 
" ============================================================================
"-- Good defaults 
set nocompatible
syntax on
filetype plugin indent on
set encoding=utf-8
scriptencoding utf-8
set showcmd
set wrap lbr
set showbreak=⇇
set omnifunc=syntaxcomplete#Complete
set number
set iskeyword=a-z,A-Z,_,.,39
set hidden
set viminfo='10,<50,s10,%,h,n~/.viminfo
set nospell
set relativenumber
set shell=/bin/zsh
set foldenable " Don't fold by default

"-- Search options 
set incsearch " Search as you type
set gdefault " By default, replace all matches on a line (i.e. always s///g)
set hlsearch " Highlight search results
set ignorecase
set smartcase
set magic
set backspace=indent,eol,start

"-- Various coding preferences 
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoread
set clipboard=unnamed
set lazyredraw
set laststatus=2
set nrformats=
set title
set sidescrolloff=15
set sidescroll=1

"-- Put all temp files in one place 
set backup
set backupdir=~/.backup,.
set directory=~/.temp,.

"-- Wildmenu config 
set wildmenu
set wildmode=list:longest

"""" Ignore certain files and directories in Wildmenu
set wildignore=*.o,*.obj,*~ 
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif

" }}}
" ============================================================================
" PLUGINS {{{ 
" ============================================================================
call plug#begin('~/.vim/plugged')

" Languages
Plug 'fatih/vim-go'
Plug 'pangloss/vim-javascript'
Plug 'racer-rust/vim-racer'
Plug 'plasticboy/vim-markdown'

" Utility
Plug 'shime/vim-livedown'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'Konfekt/FastFold'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'dahu/vim-fanfingtastic'
Plug 'easymotion/vim-easymotion'
Plug 'ervandew/supertab'
Plug 'garbas/vim-snipmate'
Plug 'godlygeek/tabular'
Plug 'guns/vim-sexp'
Plug 'honza/vim-snippets'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'kien/ctrlp.vim'
Plug 'lervag/vimtex'
Plug 'majutsushi/tagbar'
Plug 'nvie/vim-flake8'
Plug 'rking/ag.vim'
if v:version < 800
    Plug 'scrooloose/syntastic'
else
    Plug 'w0rp/ale'
endif
Plug 'tacahiroy/ctrlp-funky'
Plug 'terryma/vim-expand-region'
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'vim-scripts/a.vim'
Plug 'vim-scripts/utl.vim'
Plug 'wellle/targets.vim'

" Plugins I'm currently *not* using
" Distraction-free writing
if 1
    Plug 'junegunn/goyo.vim'
    Plug 'junegunn/limelight.vim'
endif

" Lispy stuff
if 0
    Plug 'jpalardy/vim-slime'
    Plug 'tpope/vim-sexp-mappings-for-regular-people'
endif

" Utility
if 0
    Plug 'mattn/emmet-vim'
    Plug 'dhruvasagar/vim-table-mode'
endif

call plug#end()



" }}}
" ============================================================================
" APPEARANCE {{{ 
" ============================================================================
if has('gui')
    set encoding=utf-8
    set bg=dark
    colorscheme lucius
    set guifont=Iosevka:h15
    
    " Disable menu bollocks
    set guioptions-=m
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
else
    set bg=dark
    colorscheme seoul256
    set t_ut=
endif

"-- Highlight long rows ----- 
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)
" }}}
" ============================================================================
" ABBREVIATIONS {{{ 
" ============================================================================

" Useful abbreviations
cnoreabbrev E e
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev QA qa

iabbrev @@ c.jr.davison@gmail.com
iabbrev GNSS goods and services
iabbrev GNS good and service
iabbrev TD **TODO**
iabbrev WIP **WIP**
iabbrev EX **EXAMPLE:**
iabbrev E: Explain:
iabbrev Q? **Q?** 
" }}}
" ============================================================================
" KEYBINDS {{{ 
" ============================================================================

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

" Quicker search/replace
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s///g<LEFT><LEFT>
vmap S :s///g<LEFT><LEFT>

" Selecting and Pasting
" Paste and move to end
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Select what was pasted
noremap gV `[v`]

" Select ALL
nnoremap <leader>a ggVG

" Navigate quickfix/locationlist with keychords
" if empty(getloclist(0))
"     nnoremap <C-S-n> :cn<CR>
"     nnoremap <C-S-p> :cp<CR>
" else
"     nnoremap <C-S-n> :lnext<CR>
"     nnoremap <C-S-p> :lprev<CR>
" endif

" Buffer/File/Function/Outline navigation (CtrlP versus FZF)

if has('gui_running')
    nnoremap <leader>b  :CtrlPBuffer<Cr>
    nnoremap <leader>p  :CtrlP<Cr>
else
    nnoremap <leader>b :Buffers<Cr>
    nnoremap <C-p> :Files<Cr>
    nnoremap <leader>p :Files<Cr>
endif

" Funky (CtrlP for functions)
nnoremap <leader>fu :CtrlPFunky<Cr>
nnoremap <leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>

" Keybinds to manipulate my vim config
nnoremap <leader>ev :e $MYVIMRC<Cr>
nnoremap <leader>sv :so $MYVIMRC<Cr>

nnoremap <leader>et :e ~/.vim/vimrc/totidy.vim<Cr>Go
nnoremap <leader>V :e ~/.vim/vimrc/

" Keybinds to go to specific files/dirs
nnoremap <leader>es :e $SRCME/
nnoremap <leader>ee :e $HOME/dev/etc/
nnoremap <leader>ei :e $HOME/Dropbox/notes/inbox.md<Cr>

" Miscellany
" Toggle hidden character visibility with
nmap <Leader>h :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬

" Toggle Vimtex Table of Contents
" nnoremap <leader>v :VimtexTocToggle<CR>

" Toggle tagbar
nnoremap <leader>t :TagbarToggle<CR>


nnoremap <silent> <leader>/ :nohlsearch<CR>

" Use '//' in visual mode to search for selection
vnoremap // y/<C-R>"<CR>

nnoremap <silent>/ /\v

" <leader>e -- edit file, starting in same directory as current file
" perhaps not needed...using autochdir, so ':e' will use curdir
" nmap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

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

" Fold HTML tags
nnoremap <leader>ft Vatzf

" Format a paragraph
nnoremap <leader>q gqip
" }}}
" ============================================================================
" FZF {{{ 
" ============================================================================
" --column: Show column number
" --line-number: Show line number
" --no-heading: Do not show file headings in results
" --fixed-strings: Search term as a literal string
" --ignore-case: Case insensitive search
" --no-ignore: Do not respect .gitignore, etc...
" --hidden: Search hidden files and folders
" --follow: Follow symlinks
" --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
" --color: Search color options

command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
set grepprg=rg\ --vimgrep

nmap <leader>b :Buffers<CR>
nmap <leader>f :Files<CR>
nmap <leader>F :Find 
" }}}
" ============================================================================
" LANGUAGES {{{ 
" ============================================================================
augroup filetype_c
    autocmd!
    autocmd FileType c       set foldmethod=syntax
    autocmd FileType cpo     set foldmethod=syntax
    autocmd FileType arduino set foldmethod=syntax
augroup END

augroup filetype_py
    autocmd!
    autocmd FileType python  set foldmethod=indent
    autocmd FileType python  set tabstop=4
    autocmd FileType python  set softtabstop=4
augroup END

augroup filetype_json
    autocmd!
    autocmd FileType json    set tabstop=2
    autocmd FileType json    set softtabstop=2
    autocmd FileType json    set shiftwidth=2
augroup END

augroup filetype_go
    autocmd!
    autocmd FileType go      set nofen
    autocmd FileType go      set foldmethod=syntax
augroup END

augroup pandoc
    autocmd!
    autocmd Filetype markdown,pandoc setlocal wrap textwidth=80
    autocmd Filetype markdown,pandoc setlocal conceallevel=2
    autocmd Filetype markdown,pandoc hi Conceal cterm=NONE ctermbg=NONE
    autocmd Filetype markdown,pandoc hi Conceal guibg=NONE guifg=NONE
    autocmd BufEnter *.md setlocal foldexpr=MarkdownLevel()
    autocmd BufEnter *.md setlocal foldmethod=expr
augroup END

" Open Relative Markdown Links 
" function! OpenRelativeMarkdownLink()
"     normal vi]y
"     normal /\[<C-R>"\]:
"     normal f:W
"     normal :call pandoc#hypertext#OpenLink( g:pandoc#hypertext#edit_open_cmd )<Cr>
"     normal N:noh<Cr>
" endfunction

"nnoremap grl vi]y/\[<C-R>"\]<CR>f:W:call OpenLink()<cr>N:noh<cr>

" Function for markdown folding 
function! MarkdownLevel()
    let h = matchstr(getline(v:lnum), '^#\+')
    if empty(h)
        return "="
    endif
    return ">" . len(h)
endfunction

" Generate a MD preview for the current file 
function! MDPreview()
    silent !clear
    let frm = '--from markdown_github+yaml_metadata_block+raw_html'
    let cfg = '--toc --toc-depth=2 --mathjax -s --self-contained'
    let style = '-c ~/.dotfiles/github-markdown.css'
    let out = '-o ~/.mdpreview.html'
    let str = '!pandoc %' . ' ' . frm . ' ' . cfg . ' ' . style . ' ' . out
    " echo str
    execute str
endfunction

" Tidy up the current markdown file 
function! MDTidy()
    silent !clear
    let ext = 'markdown+yaml_metadata_block+tex_math_dollars+line_blocks'
    let to = '--to=' . ext
    let extra = '--atx-headers --wrap=None --normalize --standalone'
    let out = '-o %'
    let mdtidy_command = 'pandoc % ' . to . ' ' . extra . ' ' . out
    execute "!" . mdtidy_command
endfunction

function! MDTidyWrap()
    silent !clear
    let ext = 'markdown+yaml_metadata_block+tex_math_dollars+line_blocks'
    let to = '--to=' . ext
    let extra = '--atx-headers --columns=80 --normalize --standalone'
    let out = '-o %'
    let mdtidy_command = 'pandoc % ' . to . ' ' . extra . ' ' . out
    execute "!" . mdtidy_command
endfunction

" Convert current markdown file to PDF 
function! MDToPDF()
    silent !clear
    let outfn=expand('%:r') . '.pdf'
    let cmd = 'pandoc % -o ' . outfn
    execute "!" . cmd
endfunction

command! MDTidy call MDTidyWrap()
command! MDToPDF call MDToPDF()
command! MDPreview call MDPreview()

" Miscellany 
augroup filetype_miscellany
    autocmd!
    autocmd BufNewFile,BufReadPost *.tex set filetype=tex
    autocmd FileType make    set noexpandtab
    autocmd FileType rust    set foldmethod=syntax
    autocmd FileType vim     set foldmethod=marker
    autocmd BufNewFile,BufReadPost *.es6 set filetype=javascript
    autocmd BufEnter * hi vimOper cterm=NONE ctermbg=NONE
    autocmd BufEnter * hi vimOper guibg=NONE guifg=NONE
augroup END

" Settings --- 'let' commands 
let b:javascript_fold=1
let g:go_fmt_command = "goimports"

" Tagbar support
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

" Tables
let g:table_mode_corner="|"
let g:table_mode_corner_corner="|"
let g:table_mode_header_fillchar="-"

let g:tex_flavor='latex'

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpForwardTrigger="<c-z>"
let g:UltiSnipsEditSplit="vertical"


let g:pandoc#spell#enabled=0
let g:pandoc#syntax#conceal#urls = 1
let g:pandoc#formatting#mode='s'
let g:pandoc#formatting#textwidth=0
let g:pandoc#formatting#equalprg = "pandoc -t markdown -s"
let g:pandoc#formatting#extra_equalprg = "--columns=80 --normalize --atx-headers"
let g:pandoc#syntax#conceal#blacklist = ['list', 'atx']
let g:vim_markdown_toc_autofit = 1

" }}}
" ============================================================================
" SHEBANG {{{ 
" ============================================================================
" Automatically add HashBang lines -----
function! Hashbang(portable, permission)
let shells = {
        \    'awk': "awk",
        \     'sh': "bash",
        \     'hs': "runhaskell",
        \     'jl': "julia",
        \    'lua': "lua",
        \    'mak': "make",
        \     'js': "node",
        \      'm': "octave",
        \     'pl': "perl",
        \    'php': "php",
        \     'py': "python",
        \      'r': "Rscript",
        \     'rb': "ruby",
        \  'scala': "scala",
        \    'tcl': "tclsh",
        \    ' tk': "wish"
        \    }

let extension = expand("%:e")

if has_key(shells,extension)
    let fileshell = shells[extension]

    if a:portable
        let line =  "#! /usr/bin/env " . fileshell
    else
        let line = "#! " . system("which " . fileshell)
    endif

    0put = line

    if a:permission
        :autocmd BufWritePost * :autocmd VimLeave * :!chmod u+x %
    endif

endif

endfunction

autocmd! BufNewFile *.* :call Hashbang(1,1)


" }}}
" ============================================================================
" STATUSBAR {{{ 
" ============================================================================
" Vim status bar {{{
" %< Where to truncate
" %n buffer number
" %F Full path
" %m Modified flag: [+], [-]
" %r Readonly flag: [RO]
" %y Type:          [vim]
" fugitive#statusline()
" %= Separator
" %-14.(...)
" %l Line
" %c Column
" %V Virtual column
" %P Percentage
" %#HighlightGroup#
if 1
    set statusline=%<[%n]\ %F\ %m%r%y\ %{exists('g:loaded_fugitive')?fugitive#statusline():''}\ %=%-14.(%l,%c%V%)\ %P
    silent! if emoji#available()
      let s:ft_emoji = map({
        \ 'c':          'baby_chick',
        \ 'clojure':    'lollipop',
        \ 'coffee':     'coffee',
        \ 'cpp':        'chicken',
        \ 'css':        'art',
        \ 'eruby':      'ring',
        \ 'gitcommit':  'soon',
        \ 'haml':       'hammer',
        \ 'help':       'angel',
        \ 'html':       'herb',
        \ 'java':       'older_man',
        \ 'javascript': 'monkey',
        \ 'make':       'seedling',
        \ 'markdown':   'book',
        \ 'perl':       'camel',
        \ 'python':     'snake',
        \ 'ruby':       'gem',
        \ 'scala':      'barber',
        \ 'sh':         'shell',
        \ 'slim':       'dancer',
        \ 'text':       'books',
        \ 'vim':        'poop',
        \ 'vim-plug':   'electric_plug',
        \ 'yaml':       'yum',
        \ 'yaml.jinja': 'yum'
        \ }, 'emoji#for(v:val)')

        function! S_filetype()
        if empty(&filetype)
          return emoji#for('grey_question')
        else
          return get(s:ft_emoji, &filetype, '['.&filetype.']')
        endif
      endfunction

      function! S_modified()
        if &modified
          return emoji#for('kiss').' '
        elseif !&modifiable
          return emoji#for('construction').' '
        else
          return ''
        endif
      endfunction

      function! S_fugitive()
        if !exists('g:loaded_fugitive')
          return ''
        endif
        let head = fugitive#head()
        if empty(head)
          return ''
        else
          return head == 'master' ? emoji#for('crown') : emoji#for('dango').'='.head
        endif
      endfunction

      let s:braille = split('"⠉⠒⠤⣀', '\zs')
      function! Braille()
        let len = len(s:braille)
        let [cur, max] = [line('.'), line('$')]
        let pos  = min([len * (cur - 1) / max([1, max - 1]), len - 1])
        return s:braille[pos]
      endfunction

      hi def link User1 TablineFill
      let s:cherry = emoji#for('cherry_blossom')
      function! MyStatusLine()
        let mod = '%{S_modified()}'
        let ro  = "%{&readonly ? emoji#for('lock') . ' ' : ''}"
        let ft  = '%{S_filetype()}'
        let fug = ' %{S_fugitive()}'
        let sep = ' %= '
        let pos = ' %l,%c%V '
        let pct = ' %P '

        return s:cherry.' [%n] %F %<'.mod.ro.ft.fug.sep.pos.'%{Braille()}%*'.pct.s:cherry
      endfunction

      " Note that the "%!" expression is evaluated in the context of the
      " current window and buffer, while %{} items are evaluated in the
      " context of the window that the statusline belongs to.
      set statusline=%!MyStatusLine()
    endif
endif
" }}}
" }}}
" ============================================================================
" SUPERTAB {{{ 
" ============================================================================
let g:SuperTabDefaultCompletionType = "context"
" }}}
" ============================================================================
" SYNTASTIC {{{ 
" ============================================================================
" set statusline+=%#warningmsg#
" set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" }}}
" ============================================================================
" VIM SLIME {{{ 
" ============================================================================
let g:slime_target = "tmux"
let g:slime_python_ipython = 1
" }}}
" ============================================================================
" GOYO -- Distraction free writing {{{ 
" ============================================================================
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!
let g:goyo_width=80
" }}}
" ============================================================================
" CUSTOM FOLD {{{ 
" ============================================================================
" Custom fold
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


" }}}
" ============================================================================
" CUSTOM FUNCS {{{ 
" ============================================================================
fu! ToggleWrap()
    let wr=&wrap
    if wr
        set nowrap
    else
        set wrap
    endif
endfu

nmap nw :call ToggleWrap()<CR>

fu! CopyFilename()
    let @+=expand("%")
endfu

nnoremap fmt :normal "ggVG="<Cr>
" }}}
" ============================================================================
" TO TIDY -- Experimental stuff that may not stay {{{ 
" ============================================================================
" Latex / Vimtex 
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
let g:tex_flavor = "latex"
let g:vimtex_indent_enabled=1
let g:vimtex_fold_enabled=1

" C++ 
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

" Python 
let g:pymode_python = 'python3'

let g:syntastic_python_python_exec = '/usr/local/bin/python3'
let g:syntastic_python_checkers = ['flake8']

" Rust 
let g:racer_cmd = "/Users/davison/prog/z__NOT_MINE/racer/target/release/racer"
let $RUST_SRC_PATH="/Users/davison/prog/z__NOT_MINE/rust_1.3_src/src/"

" The Silver Searcher (Ag) for grep 
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif
if executable('rg')
  set grepprg=rg\ --vimgrep
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
endif

nnoremap <Leader>g :Ag<SPACE>

" Uppercase the previous WORD while in normal mode
nnoremap <c-u> viwUE

function! OpenScopesSnippets()
    let ft = &filetype
    let dr = expand('~/.vim/snippets/')
    let fn = dr . ft . '.snippets'
    execute "e " . fn
endfunction
nnoremap <leader>os mZ:call OpenScopesSnippets()<Cr>

map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

nnoremap <F11> :Goyo<Cr>

" }}}
" ============================================================================
