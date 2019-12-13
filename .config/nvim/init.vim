let mapleader=" "

" plugins, managed with github.com/junegunn/plug.vim
" --------------------------------------------------
call plug#begin('~/.vim/3rd_party')
" languages
Plug 'fatih/vim-go'
Plug 'lervag/vimtex'
Plug 'plasticboy/vim-markdown'
Plug 'vim-jp/vim-cpp'
Plug 'vim-python/python-syntax'
Plug 'georgewitteman/vim-fish'
Plug 'guns/vim-clojure-static'
Plug 'elixir-lang/vim-elixir'
Plug 'ekalinin/Dockerfile.vim'
Plug 'vim-scripts/gnuplot-syntax-highlighting'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'vim-perl/vim-perl'
Plug 'vim-ruby/vim-ruby'
Plug 'vim-scripts/R.vim'
" Utility
Plug 'airblade/vim-gitgutter'         " Highlight uncommitted changes on left edge
Plug 'michaeljsmith/vim-indent-object'
Plug 'Konfekt/FastFold'
Plug 'dahu/vim-fanfingtastic'
Plug 'easymotion/vim-easymotion'      " Easily navigate to any word or char in buffer
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
Plug 'jceb/vim-textobj-uri'           " Text object for urls
Plug 'junegunn/fzf', { 'dur':  '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'kana/vim-textobj-user'          " Custom text objects
Plug 'ludovicchabant/vim-gutentags'   " Automatically regenerate tags
Plug 'majutsushi/tagbar'              " Show an interactive list of tags
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'romainl/vim-qlist'              " add [I results to quickfix
Plug 'Shougo/echodoc'
Plug 'tpope/vim-commentary'           " Comment modification/text objects
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'             " 'Surround' text objects e.g. csi(
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'              " Easily navigate directories
Plug 'wellle/targets.vim'
" Themes
Plug 'junegunn/seoul256.vim'
Plug 'owickstrom/vim-colors-paramount'
call plug#end()

" settings
" --------
set nocompatible
set wrap lbr
let &showbreak = '>>'
set cpo+=n
set autochdir
set breakindent
set breakindentopt+=shift:2,sbr
set number
set iskeyword=a-z,A-Z,_  " Used e.g. when searching for tags
set updatetime=300 " Write a swap file after 1 second
set cmdheight=1
set colorcolumn=0
set ignorecase smartcase " ignore case unless i specifically mix letter case
set tabstop=4 softtabstop=4 shiftround shiftwidth=4 expandtab
set clipboard=unnamedplus " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw while executing macros

" Some servers have issues with backup files
set nobackup nowritebackup

set directory=~/.temp,.
set wildmode=list:longest,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~
set wildignorecase
set nojoinspaces   " don't autoinsert two spaces after '.' etc in join
set switchbuf=useopen,usetab
set splitbelow splitright
set relativenumber
set noshowmode
let g:netrw_list_hide= '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$'

" suppress 'match x of y', 'only match'... etc
set shortmess=a

set signcolumn=yes

set path+=**
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
set bg=dark
silent! colorscheme paramount

" settings for plugins
" --------------------
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'rust', 'go', 'c', 'cpp']
let g:vim_markdown_folding_disabled = 1
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
if has('nvim')
    nnoremap <leader>ev :edit ~/.config/nvim/init.vim<CR>
else
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
nnoremap D      dd
nnoremap k      gk
nnoremap Y y$
nnoremap <silent> <CR> :nohlsearch<CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Close quickfix or location window
nnoremap <leader>c :cclose<bar>lclose<CR>

" keybinds for installed plugins
nnoremap <leader>en :Files ~/Dropbox/notes/<CR>
nnoremap <leader>es :Files ~/src/github.com/ChrisDavison/scripts<CR>
nnoremap <leader>el :Files ~/src/github.com/ChrisDavison/logbook/2019<CR>
nnoremap <leader>p :call <SID>maybe_gfiles()<CR>


function! s:maybe_gfiles()
    let root = split(system('git rev-parse --show-toplevel'), '\n')[0]
    if !v:shell_error
        GFiles
    else
        Files
    end
endfunction

nnoremap <leader>b :Buffers<CR>

command! TagThisWord exec "Tags " . expand("<cword>") 
nnoremap <leader>gt :TagThisWord<CR>

nmap s <Plug>(easymotion-s)
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

nnoremap <leader>t :Tags<CR>

" custom commands
" ---------------
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! CD exec "cd ".expand("%:h")

function! s:note(fn)
    exec "e ~/Dropbox/notes/_UNFILED/" . a:fn . ".txt" 
endfunction
command! -nargs=1 Note call s:note(<args>)

" :Bd | Delete buffer and replace with 'alternate' buffer
" -------------------------------------------------------
command! Bd bp|bd #

" :Scratch | Open a 'scratch' buffer
" ----------------------------------
command! Scratch edit ~/.scratch | normal G
nnoremap <leader>s  :Scratch<CR>

" :FMT | Execute 'equalprg' on entire buffer, remembering position
" ----------------------------------------------------------------
command! FMT exec "silent!normal mzgg=G`zmzzz"
nnoremap <leader>f :FMT<CR>

" :MakeNonExistentDir | try to make all parent directories of a new buffer
" ------------------------------------------------------------------------
function! s:makeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction
command! MakeNonExistentDir call s:makeNonExDir()

" grep / ripgrep
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
" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

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

" :EX | chmod +x
" --------------
command! EX if !empty(expand('%'))
         \|   write
         \|   call system('chmod +x '.expand('%'))
         \|   silent e
         \| else
         \|   echohl WarningMsg
         \|   echo 'Save the file first'
         \|   echohl None
         \| endif

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

" :Mkdp | Wrapper for MarkdownPreview, so that I can call it from txt files
" -------------------------------------------------------------------------
function! s:Mkdp(bang)
    if a:bang
        call mkdp#util#stop_preview()
    else
        call mkdp#util#open_preview_page()
    endif
endfunction
command! -bang Mkdp call s:Mkdp(<bang>0)

" Commands to jump to specific files or directories
" -------------------------------------------------
command! Inbox exec "edit " . expand('$HOME/Dropbox/notes/inbox.txt')
command! Someday exec "edit " . expand('$HOME/Dropbox/notes/someday.txt')
command! Projects exec "edit " . expand('$HOME/Dropbox/notes/projects')
command! Logbook exec "edit " . expand('$HOME/src/github.com/ChrisDavison/logbook/' . strftime("%Y"))

" abbreviations
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
cnoreabbrev Q! q!
cnoreabbrev GIt Git
cnoreabbrev Set set

iabbrev meanstd μ±σ
iabbrev SALS **See also**:
iabbrev <expr> DATE strftime("%Y%m%d")
iabbrev <expr> DATETIME strftime("%Y-%m-%dT%H:%M:%S")
iabbrev RSQ R²


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

" autocommands
" ------------
augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd BufWritePre * call s:makeNonExDir()
    au ColorScheme * hi! link SignColumn LineNr
    au CursorHold * silent! checktime " Check for external changes to files
    au CursorHold * silent call CocActionAsync('highlight')
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au BufEnter .scratch set filetype=markdown
    au BufEnter *.txt,*.md set filetype=markdown
    au Filetype arduino set filetype=cpp
    au Filetype make setlocal noexpandtab
    au Filetype markdown setlocal equalprg=pandoc\ --to\ markdown-shortcut_reference_links+pipe_tables-simple_tables-fenced_code_attributes\ --columns=80\ --reference-links\ --reference-location=section\ --wrap=auto\ --atx-headers
    au Filetype markdown nnoremap <buffer> <leader>i :g/^#\+\s<CR>:
    au Filetype markdown :silent! CocDisable
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex setlocal tw=80
    au Filetype tex setlocal colorcolumn=80
    au Filetype tex setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
    au Filetype python nnoremap <buffer> <leader>i :g/^def\s<CR>:
    au FileType python let b:coc_root_patterns = ['.env', '.git']
    au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END
