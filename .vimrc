let mapleader=" "

" Load plugins
" }}}1 plugins (using junegunn's Plug.vim) {{{1

call plug#begin('~/.vim/bundle')
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
Plug 'tpope/vim-speeddating'      
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'      
Plug 'vim-python/python-syntax'
Plug 'wellle/targets.vim'
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
Plug 'ChrisDavison/checkmark'
Plug 'ChrisDavison/vim-cdroot'
call plug#end()

" }}}1 settings {{{1
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

" }}}1 appearance {{{1
" when do I need termguicolours? why did I switch it off?
" problem between vim and neovim? terminal and gui? windows vs osx?
if !has('mac')
    set termguicolors
endif
set t_ut= " Fix issues with background color on some terminals
set t_Co=256
set bg=dark
silent! colorscheme base16-material

" settings for plugins {{{1

let g:markdown_fenced_languages = ['python', 'rust', 'cpp', 'go']
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
" chrisdavison/vim-cdroot -  Priority curdirs if in a subdir
let g:non_git_roots=["~/Dropbox/notes", "~/Dropbox/logbook"]

" }}}1 keybinds {{{1
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

nnoremap <leader>ev :edit ~/.vimrc<CR>
nnoremap <leader>en :Files! ~/Dropbox/notes/<CR>
nnoremap <leader>es :Files! ~/src/github.com/ChrisDavison/scripts<CR>
nnoremap <leader>el :Files! ~/Dropbox/logbook/2020/<CR>
nnoremap <leader>b :Buffers!<CR>
nnoremap <leader>l :BLines!<CR>

command! MGFiles call s:maybe_gfiles()
nnoremap <leader>p :MGFiles<CR>


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
nnoremap <leader>T :BTags<CR>

" }}}1 abbreviations {{{1
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
cnoreabbrev Q! q!
cnoreabbrev GIt Git
cnoreabbrev Set set
cnoreabbrev oedit only<bar>edit
cnoreabbrev oe only<bar>edit

" :BufOnly | Close all buffers but this one
cnoreabbrev BufOnly %bd\|e#

iabbrev meanstd μ±σ
iabbrev SALS **See also**:
iabbrev <expr> DATE strftime("%Y-%m-%d")
iabbrev <expr> DATEN strftime("%Y-%m-%d %A")
iabbrev <expr> DATED strftime("%b %d")
iabbrev <expr> DATEFULL strftime("%Y-%m-%d %A")
iabbrev <expr> DATENFULL strftime("%Y %b %d")
iabbrev <expr> jhead strftime("# %Y-%m-%d")
iabbrev <expr> TIME strftime("%H:%M:%S")
iabbrev RSQ R²
iabbrev pmin1 ⁻¹

" }}}1 :CD | Change to the parent directory of the current file {{{1
command! CD exec "cd ".expand("%:h")

" }}}1 :Bd | Delete buffer and replace with 'alternate' buffer {{{1
command! Bd bp|bd #

" }}}1 :Scratch | Open a 'scratch' buffer {{{1
command! Scratch edit ~/.scratch | normal <C-End>

" }}}1 :FMT | Execute 'equalprg' on entire buffer, remembering position {{{1
command! FMT exec "normal mzgg=G`zmzzz"
nnoremap <leader>f :FMT<CR>

" }}}1 :MakeNonExistentDir | try to make all parent directories of a new buffer {{{1
function! s:makeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction
command! MakeNonExistentDir call s:makeNonExDir()

" }}}1 :RG | grep / ripgrep {{{1
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
    command! -bang -nargs=* RG
                \ call fzf#vim#grep(
                \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
                \ fzf#vim#with_preview('right:50%:hidden', '?'),
                \ <bang>0)
endif
" }}}1 :Headers | imenu-like list functions,headers etc, for defined filetypes {{{1
let s:headermap={
            \'rust': 'fn',
            \'python': 'def',
            \'go': 'func',
            \'vim': 'function',
            \'markdown': '#\+',
            \'markdown.pandoc': '#\+'}
function! s:goto_header(ft, filter)
    let pattern="^\\s*" . s:headermap[a:ft] . "\\s"
    if len(a:filter) > 0
        let pattern= l:pattern . a:filter 
    endif
    exec ":g/" . pattern . "/"
endfunction
command! -nargs=* Headers exec s:goto_header(&filetype, <q-args>)
nnoremap <leader>i :Headers<CR>:
nnoremap <leader>I :Headers !<CR>:
" }}}1 Markdown {{{1
let markdown_reference_links=1
let markdown_hard_wrap=0
let md_equalprg="pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block"
let md_equalprg.=markdown_reference_links ? "-shortcut_reference_links\ --reference-links\ --reference-location=section" : ""
let md_equalprg.=markdown_hard_wrap ? "\ --columns=79\ --wrap=auto" : "\ --wrap=none"
let md_equalprg.="\ --atx-headers"

function! s:maybe_filetype_markdown()
    if &filetype == "help" || expand('%:p') =~ "doc/"
        setlocal filetype=help
        return
    endif
    setlocal filetype=markdown
endfunction

function! MarkdownLevel() "folding function
    if getline(v:lnum) =~ '^# .*$'
        return ">1"
    endif
    if getline(v:lnum) =~ '^## .*$'
        return ">2"
    endif
    if getline(v:lnum) =~ '^### .*$'
        return ">3"
    endif
    if getline(v:lnum) =~ '^#### .*$'
        return ">4"
    endif
    if getline(v:lnum) =~ '^##### .*$'
        return ">5"
    endif
    if getline(v:lnum) =~ '^###### .*$'
        return ">6"
    endif
    return "=" 
endfunction


function MarkdownFoldtext()
    let l1 = getline(v:foldstart)
    if l:l1[0] != '#'
        return repeat('#', v:foldlevel) . ' ' . l:l1 . '...'
    else
        return l:l1 . '   «' . (v:foldend - v:foldstart) . '» '
    endif
endfunction
" }}}1 autocommands {{{1
augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd BufWritePre * call s:makeNonExDir()
    " au ColorScheme * hi! link SignColumn LineNr
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au BufEnter *.vim setlocal foldmethod=marker
    au BufEnter *.txt,*.md,.scratch call s:maybe_filetype_markdown()
    au BufEnter * Root
    au Filetype arduino set filetype=cpp
    au Filetype make setlocal noexpandtab
    au Filetype markdown setlocal foldenable foldlevelstart=0
    au Filetype markdown setlocal conceallevel=1
    au Filetype markdown setlocal foldexpr=MarkdownLevel()  
    au Filetype markdown setlocal foldmethod=expr
    au Filetype markdown setlocal foldtext=MarkdownFoldtext()
    au Filetype markdown setlocal nospell
    au Filetype markdown let &l:equalprg=md_equalprg
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex setlocal tw=80 colorcolumn=80
    au Filetype tex setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
    au FileType python setlocal foldmethod=indent
augroup END
" }}}1

" vim:set foldmethod=marker
