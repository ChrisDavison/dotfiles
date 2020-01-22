let mapleader=" "

" pluginslogs/, managed with github.com/junegunn/plug.vim
" --------------------------------------------------
call plug#begin('~/.vim/3rd_party')
" languages
Plug 'fatih/vim-go'
Plug 'lervag/vimtex'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-python/python-syntax'
Plug 'georgewitteman/vim-fish'
Plug 'ekalinin/Dockerfile.vim'
Plug 'airblade/vim-gitgutter'        
Plug 'michaeljsmith/vim-indent-object'
Plug 'Konfekt/FastFold'
Plug 'dahu/vim-fanfingtastic'
Plug 'easymotion/vim-easymotion'  
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
Plug 'junegunn/fzf', { 'dir':  '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'kana/vim-textobj-user'     
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'          
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'romainl/vim-qlist'         
Plug 'Shougo/echodoc'
Plug 'tpope/vim-commentary'     
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'      
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'      
Plug 'wellle/targets.vim'
Plug 'junegunn/seoul256.vim'
Plug 'owickstrom/vim-colors-paramount'
call plug#end()

" settings
" --------
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
set clipboard=unnamedplus " Use system clipboard with vim clipboard
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
" set relativenumber
set noshowmode
let g:netrw_list_hide= '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$,.mypy_cache,__pycache__'

" suppress 'match x of y', 'only match'... etc
set shortmess=a

set signcolumn=auto

set path=.,**
set statusline=%<\ %n:%f\ %m%r%y%=%(%P\ of\ %LL\ -\ %l,%c\ %)

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
" ----------
" when do I need termguicolours? why did I switch it off?
" problem between vim and neovim? terminal and gui? windows vs osx?
if !has('mac')
    set termguicolors
endif
set t_ut= " Fix issues with background color on some terminals
set t_Co=256
set bg=light
silent! colorscheme paramount

" settings for plugins
" --------------------
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
let g:tagbar_type_markdown = {
            \ 'ctagstype': 'markdown',
            \ 'ctagsbin' : '/home/davison/.local/bin/markdown2ctags',
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

"  keybinds
"  --------
nnoremap <leader>ev :edit $MYVIMRC<CR>

nnoremap <silent> Q =ip
nnoremap <BS>   <C-^>
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

" keybinds for installed plugins
nnoremap <leader>en :Files ~/Dropbox/notes/<CR>
nnoremap <leader>es :Files ~/src/github.com/ChrisDavison/scripts<CR>
nnoremap <leader>el :Files ~/Dropbox/logbook/2020<CR>

command! MGFiles call s:maybe_gfiles()
nnoremap <leader>p :MGFiles<CR>
nnoremap <leader>b :Buffers<CR>

function! s:maybe_gfiles()
    " Root is only called to test for it's error code
    let root = split(system('git rev-parse --show-toplevel'), '\n')[0]
    if expand('%:p') =~ "Dropbox/notes"
        exec "Files " . expand("~/Dropbox/notes")
    elseif expand('%:p') =~ "Dropbox/logbook"
        exec "Files " . expand("~/Dropbox/logbook")
    elseif !v:shell_error
        GFiles
    else
        Files
    end
endfunction

" Easymotion configuration
nmap s <Plug>(easymotion-s)
nmap <leader><leader>w <Plug>(easymotion-bd-w)
nmap <leader><leader>e <Plug>(easymotion-bd-e)
map <leader>j <Plug>(easymotion-bd-jk)
map <leader>k <Plug>(easymotion-bd-jk)
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
" ---------------
command! CopyFilename exec "@+=expand(\"%:p\")"
command! CopyRelativeFilename exec "@+=expand(\"%\")"

" :CD | Change to the parent directory of the current file
" --------------------------------------------------------
command! CD exec "cd ".expand("%:h")

" :Note | Create a new note in Dropbox/notes/_UNFILED, with the given text
" ------------------------------------------------------------------------
function! s:note(fn)
    exec "e ~/Dropbox/notes/_UNFILED/" . substitute(a:fn, " ", "-", "g") . ".txt" 
endfunction
command! -nargs=+ Note call s:note(<args>)

" :Bd | Delete buffer and replace with 'alternate' buffer
" -------------------------------------------------------
command! Bd bp|bd #

" :Scratch | Open a 'scratch' buffer
" ----------------------------------
command! Scratch edit ~/.scratch | normal <C-End>

" :FMT | Execute 'equalprg' on entire buffer, remembering position
" ----------------------------------------------------------------
command! FMT exec "normal mzgg=G`zmzzz"
nnoremap <leader>f :FMT<CR>

" :MakeNonExistentDir | try to make all parent directories of a new buffer
" ------------------------------------------------------------------------
function! s:makeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction
command! MakeNonExistentDir call s:makeNonExDir()

" :Rg | grep / ripgrep
" --------------
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
    command! -bang -nargs=* Rg
                \ call fzf#vim#grep(
                \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
                \ fzf#vim#with_preview('right:50%:hidden', '?'),
                \ <bang>0)
endif

" coc.nvim, the nvim language server protocol runner
" --------------------------------------------------
" keybinds for Coc.nvim, the completion engine
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

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
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

" :Root | Change dir to the root of the Git repository
" ----------------------------------------------------
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

" :Mkdp[!] | Wrapper for MarkdownPreview, so that I can call it from txt files
" -------------------------------------------------------------------------
function! s:Mkdp(bang)
    if a:bang
        call mkdp#util#stop_preview()
    else
        call mkdp#util#open_preview_page()
    endif
endfunction
command! -bang Mkdp call s:Mkdp(<bang>0)

" :Autowrap[!] | Turn automatic column-80 wrapping on/off  (for md/txt only)
" --------------------------------------------------------------------------
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
command! -bang Autowrap call <sid>toggle_autowrap(<bang>0)

" :Log | Create an entry in $NOTES/logs/ with the specified name
function! s:new_personal_log(args, daily)
    if a:daily
        let filename=strftime("%Y%m%d") . "-" . join(split(a:args), "-") . ".txt"
    else
        let filename=strftime("%Y%m") . "-" . join(split(a:args), "-") . ".txt"
    endif
    let path=expand("$HOME/Dropbox/notes/logs/" . filename)
    exec "edit " . path
endfunction
command! -bang -nargs=+ Log call <sid>new_personal_log(<q-args>, <bang>0)

" 'File stacks' - open every file in a list as horizontally split buffers
" using [!] variant will make ONLY these buffers display
"
" :Habits[!] -- daily, weekly, and monthly habit checklist
" :Thesis[!] -- general, beef chapter, and dairy chapter
" :Todo[!]   -- today, personal todos, work todos
function! s:stack_open_files(files, as_split)
    if a:as_split
        exec "vert botright split " . a:files[0]
    else
        only
        exec "edit " . a:files[0]
    endif
    for fn in a:files[1:]
        exec "split " . fn
    endfor
endfunction

let s:habit_files = ['$HOME/Dropbox/notes/habits/1-daily.txt',
            \ '$HOME/Dropbox/notes/habits/2-weekly.txt',
            \ '$HOME/Dropbox/notes/habits/3-monthly.txt']
let s:thesis_files = ['$HOME/Dropbox/notes/todo/thesis-general.txt',
            \ '$HOME/Dropbox/notes/todo/thesis-chapter-dairy.txt',
            \ '$HOME/Dropbox/notes/todo/thesis-chapter-beef.txt']
let s:todo_files = ['$HOME/Dropbox/notes/todo/todo.txt' ]
let s:todo_today = '$HOME/Dropbox/notes/todo/today.txt'

command! -bang Habits silent!call <sid>stack_open_files(s:habit_files, <bang>1)
command! -bang Thesis silent!call <sid>stack_open_files(s:thesis_files, <bang>1)
command! Plan only|silent!exec "edit " . s:todo_today|silent!call <sid>stack_open_files(s:todo_files, 1)

" Commands to jump to specific files or directories
" Using my 'stack open', so that I can use the [!] variant if wanted
command! -bang Today silent!call <sid>stack_open_files(['$HOME/Dropbox/notes/todo/today.txt'], <bang>1)
command! -bang Inbox silent!call <sid>stack_open_files(['$HOME/Dropbox/notes/inbox.txt'], <bang>1)
command! -bang Someday silent!call <sid>stack_open_files(['$HOME/Dropbox/notes/todo/someday.txt'], <bang>1)
command! -bang Projects silent!call <sid>stack_open_files(['$HOME/Dropbox/notes/todo'], <bang>1)
command! -bang Todos silent!call <sid>stack_open_files(['$HOME/Dropbox/notes/todo')], <bang>1)
command! -bang Logbook silent!call <sid>stack_open_files(['$HOME/Dropbox/logbook/' . strftime("%Y")], <bang>1)


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
iabbrev <expr> DATE strftime("%Y%m%d")
iabbrev <expr> DATETIME strftime("%Y-%m-%dT%H:%M:%S")
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
" :Check will add, if none exists
" Will toggle, if exists
" Will clear, if 'off' is 1 (e.g. if :Check!)
" :Uncheck will clear all (or only selected) checkboxes
" :RMCheck will delete lines with checked boxes
function! s:checkbox_rotate()
    if getline(".") !~ "\[[x ]\]"
        silent!s/\(\s*\%(-\|[0-9]\+.\)\s\+\)\([^[].*$\)/\1[ ] \2
    elseif getline(".") =~ "\\[ \\]"
        silent!s/\[ \]/\[x\]/
    else
        silent!s/\[x\] //
    endif
endfunction
function! s:checkbox_delete()
    silent!s/\[[x ]\] //
    silent!s/aksjdasd
endfunction
command! -range CheckRot <line1>,<line2>call <sid>checkbox_rotate()
command! -range Uncheck <line1>,<line2>call <sid>checkbox_delete()
command! RMCheck :%s/\s*-\s\+\[x\].*\n\(\s*[^-]\s\+.*\n\)*//

nnoremap <leader>x :CheckRot<CR>
vnoremap <leader>x :'<,'>CheckRot<CR>
nnoremap <leader>X :Uncheck<CR>
vnoremap <leader>X :'<,'>Uncheck<CR>

" :Headers | imenu-like list functions,headers etc, for defined filetypes
" -----------------------------------------------------------------------
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
command! -nargs=* Headers exec <sid>goto_header(&filetype, <q-args>)
nnoremap <leader>i :Headers<CR>:
nnoremap <leader>I :Headers !<CR>:

function! s:set_markdown_wrap_mode(hard)
    let fmt="pandoc --to "
    let fmt=fmt + "markdown"
    setlocal equalprg=pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block
    if g:markdown_reference_links
        setlocal equalprg+=-shortcut_reference_links\ --reference-links\ --reference-location=section
    endif
    setlocal equalprg+=\ --atx-headers
    if a:hard
        setlocal equalprg+=\ --columns=79\ --wrap=auto
    else
        setlocal equalprg+=\ --wrap=none
    endif
endfunction

" autocommands
" ------------
augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd BufWritePre * call s:makeNonExDir()
    au ColorScheme * hi! link SignColumn LineNr
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au BufEnter .scratch call <sid>maybe_filetype_markdown()
    au BufEnter *.txt,*.md call <sid>maybe_filetype_markdown()
    au Filetype arduino set filetype=cpp
    au Filetype make setlocal noexpandtab
    au Filetype markdown* setlocal foldenable foldlevelstart=99
    au Filetype markdown* setlocal conceallevel=2
    au BufEnter,BufRead,BufNewFile *md,*txt call <sid>set_markdown_wrap_mode(g:markdown_hard_wrap)
    au BufEnter,BufRead,BufNewFile *md,*txt :silent! CocDisable
    au BufEnter,BufRead,BufNewFile *.md,*.txt :setlocal nospell
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex setlocal tw=80 colorcolumn=80
    au Filetype tex setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
    au FileType python let b:coc_root_patterns = ['.env', '.git']
    au FileType python setlocal foldmethod=indent
    au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    au User GoyoEnter Limelight
    au User GoyoLeave Limelight!
augroup END
