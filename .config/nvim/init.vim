let mapleader=" "

" plugins (using junegunn's Plug.vim)
call plug#begin('~/.vim/3rd_party')
Plug 'airblade/vim-gitgutter'        
Plug 'chriskempson/base16-vim'
Plug 'dahu/vim-fanfingtastic'
Plug 'easymotion/vim-easymotion'  
Plug 'fatih/vim-go'
Plug 'junegunn/fzf', { 'dir':  '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/seoul256.vim'
Plug 'kana/vim-textobj-user'     
Plug 'Konfekt/FastFold'
Plug 'lervag/vimtex'
Plug 'ludovicchabant/vim-gutentags'
Plug 'owickstrom/vim-colors-paramount'
Plug 'romainl/vim-qf'
Plug 'romainl/vim-qlist'         
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'     
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'      
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'      
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-python/python-syntax'
Plug 'wellle/targets.vim'
call plug#end()

" settings
set nocompatible
set wrap lbr
let &showbreak = '┆'
set cpo+=n
set autochdir
set breakindent
set breakindentopt=shift:4,sbr
set number
set iskeyword=a-z,A-Z,_  " Used e.g. when searching for tags
set updatetime=300 " Write a swap file after 1 second
set ignorecase smartcase " ignore case unless i specifically mix letter case
set tabstop=4 softtabstop=4 shiftround shiftwidth=4 expandtab
set clipboard+=unnamedplus " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw while executing macros

" Some servers have issues with backup files
set nobackup nowritebackup

set directory=~/.temp,.
set wildmode=list:longest:list,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~
set wildignorecase
set nojoinspaces   " don't autoinsert two spaces after '.' etc in join
set switchbuf=useopen,usetab
set splitbelow splitright
set noshowmode
let g:netrw_list_hide= '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$,.mypy_cache,__pycache__'

" suppress 'match x of y', 'only match'... etc
set shortmess=a

set signcolumn=auto

set path=.,**
set statusline=%<\ %n:%f\ %m%r%y%{ObsessionStatus('[session]')}%=%(%P\ of\ %LL\ -\ %l,%c\ %)

" undo (save undo history across sessions)
set undodir=~/.undodir
set undofile
set completeopt=menu,menuone,preview

" shell (specialised per os)
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
else
    let shells=['/usr/bin/fish', '/usr/bin/zsh', '/usr/bin/bash', '/bin/bash']
    for possibleshell in shells
        if executable(possibleshell)
            exec "set shell=".possibleshell
            break
        endif
    endfor
endif

if has('nvim')
    set inccommand=nosplit  " Live-preview of :s commands
endif

" appearance
" when do I need termguicolours? why did I switch it off?
" problem between vim and neovim? terminal and gui? windows vs osx?
if !has('mac')
    set termguicolors
endif
set t_ut= " Fix issues with background color on some terminals
set t_Co=256
set bg=dark
silent! colorscheme base16-dracula

" settings for plugins
let g:pandoc#syntax#conceal#use=1
let g:pandoc#syntax#conceal#urls=1
let g:pandoc#syntax#conceal#blacklist=[ 'atx', 'list', 'ellipses', 'quotes' ]
let g:pandoc#syntax#style#use_definition_lists = 0
let g:pandoc#folding#mode='syntax'
let g:pandoc#folding#level=99
let g:pandoc#folding#fastfolds=1
let g:pandoc#folding#fdc=1
let g:pandoc#formatting#textwidth=80
let g:pandoc#formatting#mode='s'
let g:pandoc#formatting#equalprg='pandoc' .
            \ ' --to markdown-shortcut_reference_links+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block' .
            \ ' --atx-headers'
let g:markdown_hard_wrap=0
if g:markdown_hard_wrap " If I want to use soft-wrapping, without commenting out a bunch of lines...
    let g:pandoc#formatting#mode='ha'
    let g:pandoc#formatting#equalprg=g:pandoc#formatting#equalprg . ' --columns=79\ --wrap=auto'
endif
let g:markdown_reference_links=0
if g:markdown_reference_links
    let g:pandoc#formatting#equalprg=g:pandoc#formatting#equalprg . ' --reference-links' .
                \ ' --reference-location=section' .
endif
let g:go_fmt_command="goimports"
let g:go_fmt_autosave=1
let g:go_version_warning=0
let g:pymode_python = 'python3'
let g:rustfmt_autosave=1
let g:is_bash=1
let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'
let g:echodoc#enable_at_startup=1
let g:echodoc#type="echo"

"  keybinds
"  --------

nnoremap <silent> Q =ip
nnoremap S      :%s///<LEFT>
vnoremap S      :s///<LEFT>
vnoremap <      <gv
vnoremap >      >gv
nnoremap j      gj
vnoremap j      gj
nnoremap D      dd
nnoremap k      gk
vnoremap k      gk
nnoremap Y      y$
nnoremap <silent> <CR> :nohlsearch<CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

tnoremap <Esc> <C-\><C-n>

" FZF keybinds
imap <C-x><C-k> <plug>(fzf-complete-word)
imap <C-x><C-f> <plug>(fzf-complete-path)
imap <C-x><C-j> <plug>(fzf-complete-file-ag)
imap <C-x><C-l> <plug>(fzf-complete-line)
let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-x': 'split',
            \ 'ctrl-v': 'vsplit' }

" Automatically use first spelling suggestion
nnoremap <leader>s  z=1<CR><CR>

" Close quickfix or location window
nnoremap <leader>c :cclose<bar>lclose<CR>

" Keybinds for specific files

nnoremap <leader>ev :edit $MYVIMRC<CR>
" keybinds for installed plugins
nnoremap <leader>en :Files! ~/Dropbox/notes/<CR>
nnoremap <leader>es :Files! ~/src/github.com/ChrisDavison/scripts<CR>
nnoremap <leader>el :Files! ~/Dropbox/logbook/2020/<CR>

command! MGFiles call s:maybe_gfiles()
nnoremap <leader>p :MGFiles<CR>
nnoremap <leader>b :Buffers!<CR>

" :BufOnly | Close all buffers but this one
cnoreabbrev BufOnly %bd\|e#

function! s:maybe_gfiles()
    " Root is only called to test for it's error code
    let root = split(system('git rev-parse --show-toplevel'), '\n')[0]
    if expand('%:p') =~ "Dropbox/notes"
        exec "Files! " . expand("~/Dropbox/notes")
    elseif expand('%:p') =~ "Dropbox/logbook"
        exec "Files! " . expand("~/Dropbox/logbook")
    elseif !v:shell_error
        GFiles!
    else
        Files!
    endif
endfunction

" Copy file basename
nnoremap <leader>cf :let @+=expand("%")<CR>
" Copy file full path
nnoremap <leader>cF :let @+=expand("%:p")<CR>
" Copy file full parent dir
nnoremap <leader>cd :let @+=expand("%:p:h")<CR>

" Easymotion configuration
nmap s <Plug>(easymotion-s)
nmap <leader><leader>w <Plug>(easymotion-bd-w)
nmap <leader><leader>e <Plug>(easymotion-bd-e)
nmap <leader>j <Plug>(easymotion-bd-jk)
nmap <leader>k <Plug>(easymotion-bd-jk)
let g:EasyMotion_smartcase=1

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
inoremap <C-c> <ESC>

nnoremap <leader>t :Tags<CR>

" custom commands

" :CD | Change to the parent directory of the current file
command! CD exec "cd ".expand("%:h")

" :Bd | Delete buffer and replace with 'alternate' buffer
command! Bd bp|bd #

" :Scratch | Open a 'scratch' buffer
command! Scratch edit ~/.scratch | normal <C-End>

" :FMT | Execute 'equalprg' on entire buffer, remembering position
command! FMT exec "normal mzgg=G`zmzzz"
nnoremap <leader>f :FMT<CR>

" :MakeNonExistentDir | try to make all parent directories of a new buffer
function! s:makeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction
command! MakeNonExistentDir call s:makeNonExDir()

" " :RG | grep / ripgrep
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
    command! -bang -nargs=* RG
                \ call fzf#vim#grep(
                \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
                \ fzf#vim#with_preview('right:50%:hidden', '?'),
                \ <bang>0)
endif

" :Root | Change dir to the root of the Git repository
function! s:root(quiet)
    let root = split(system('git rev-parse --show-toplevel'), '\n')[0]
    if expand('%:p') =~ "Dropbox/notes"
        exec "lcd " . expand("~/Dropbox/notes")
    elseif expand('%:p') =~ "Dropbox/logbook"
        exec "lcd " . expand("~/Dropbox/logbook")
    elseif !v:shell_error
        execute 'lcd' root
        echo 'Changed directory to: '.root
    elseif !a:quiet
        echo "Not in git repo or under a recognised 'parent'
    end
endfunction
command! Root call s:root()

" :Autowrap[!] | Turn automatic column-80 wrapping on/off  (for md/txt only)
function! s:toggle_autowrap(bang)
    if a:bang
        echom "Autowrap DISABLED"
        setlocal formatoptions-=a
        return
    endif
    let skip=['bookmark', 'self-tracking', 'budget']
    let curdir=expand('%:p:h')
    for pattern in skip
        if curdir =~ pattern
            echom "NOT autowrapping as directory `" . pattern . "` matches skip"
            return
        endif
    endfor

    if &filetype =~ "markdown"
        echom "Autowrap ENABLED"
        setlocal formatoptions+=a
    endif
endfunction
command! -bang Autowrap call s:toggle_autowrap(<bang>0)

" :Log | shortcut to creating a logbook entry
cnoreabbrev <expr> Log strftime("edit $HOME/Dropbox/logbook/%Y/%Y%m%d-")


" Commands to jump to specific files or directories
" Using my 'stack open', so that I can use the [!] variant if wanted
command! Inbox silent only<BAR>edit $HOME/Dropbox/notes/inbox.txt<bar>vsplit $HOME/Dropbox/notes/life-focus.txt
command! Logbook silent only<BAR>edit $HOME/Dropbox/logbook/
command! Journal silent only<BAR>edit $HOME/Dropbox/notes/2020-journal.txt<BAR>norm <C-End>

" abbreviations
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
cnoreabbrev Q! q!
cnoreabbrev GIt Git
cnoreabbrev Set set
cnoreabbrev oedit only<bar>edit
cnoreabbrev oe only<bar>edit

iabbrev meanstd μ±σ
iabbrev SALS **See also**:
iabbrev <expr> DATE strftime("%Y-%m-%d")
iabbrev <expr> DATEN strftime("%Y-%m-%d %A")
iabbrev <expr> DATED strftime("%b %d")
iabbrev <expr> DATEFULL strftime("%Y-%m-%d %A")
iabbrev <expr> DATENFULL strftime("%Y %b %d")
iabbrev <expr> jhead strftime("%b %d - %A")
iabbrev <expr> TIME strftime("%H:%M:%S")
iabbrev RSQ R²
iabbrev pmin1 ⁻¹


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

function! s:maybe_filetype_markdown()
    if &filetype == "help"
        return
    else
        setlocal filetype=markdown.pandoc
    end
endfunction

" HANDLING CHECKBOXES
" 
" :Check will add, if none exists
" Will toggle, if exists
" Will clear, if 'off' is 1 (e.g. if :Check!)
" :Uncheck will clear all (or only selected) checkboxes
" :RMCheck will delete lines with checked boxes
function! s:checkbox_toggle()
    if getline(".") !~ "\[[x ]\]"
        silent!s/\(\s*\%(-\|[0-9]\+.\)\s\+\)\([^[].*$\)/\1[ ] \2
    elseif getline(".") =~ "\\[ \\]"
        silent!s/\[ \]/\[x\]/
    elseif getline(".") =~ "\\[x\\]"
        silent!s/\[x\]/\[ \]/
    endif
endfunction
function! s:checkbox_delete()
    silent!s/\[[x ]\] //
    silent!/aksjdasd
endfunction
command! -range CheckToggle <line1>,<line2>call s:checkbox_toggle()
command! -range Uncheck <line1>,<line2>call s:checkbox_delete()
command! RMCheck :%Uncheck

nnoremap <leader>x :CheckToggle<CR>
vnoremap <leader>x :'<,'>CheckToggle<CR>
nnoremap <leader>X :Uncheck<CR>
vnoremap <leader>X :'<,'>Uncheck<CR>

" :Headers | imenu-like list functions,headers etc, for defined filetypes
let s:headermap={
            \'rust': 'fn',
            \'python': 'def',
            \'go': 'func',
            \'vim': 'function',
            \'markdown': '#\+',
            \'markdown.pandoc': '#\+'}
function! s:goto_header(ft, filter)
    let pattern=s:headermap[a:ft]
    if len(a:filter) > 0
        let pattern= s:headermap[a:ft] . ".*" . a:filter . ".*"
    endif
    exec ":g/^\\s*".pattern."\\s/"
endfunction
command! -nargs=* Headers exec s:goto_header(&filetype, <q-args>)
nnoremap <leader>i :Headers<CR>:
nnoremap <leader>I :Headers !<CR>:

function! s:set_markdown_wrap_mode()
    setlocal equalprg=pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block
    if g:markdown_reference_links
        setlocal equalprg+=-shortcut_reference_links\ --reference-links\ --reference-location=section
    endif
    setlocal equalprg+=\ --atx-headers
    if g:markdown_hard_wrap
        setlocal equalprg+=\ --columns=79\ --wrap=auto
    else
        setlocal equalprg+=\ --wrap=none
    endif
endfunction

" autocommands
augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd BufWritePre * call s:makeNonExDir()
    " au ColorScheme * hi! link SignColumn LineNr
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au BufEnter .scratch call s:maybe_filetype_markdown()
    au BufEnter *.txt,*.md call s:maybe_filetype_markdown()
    au BufEnter *.txt,*.md call s:root(1)
    au Filetype arduino set filetype=cpp
    au Filetype make setlocal noexpandtab
    au Filetype markdown* setlocal foldenable foldlevelstart=99
    au Filetype markdown* setlocal conceallevel=2
    au BufEnter,BufRead,BufNewFile *md,*txt call s:set_markdown_wrap_mode()
    au BufEnter,BufRead,BufNewFile *.md,*.txt setlocal nospell
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex setlocal tw=80 colorcolumn=80
    au Filetype tex setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
    au FileType python setlocal foldmethod=indent
augroup END
