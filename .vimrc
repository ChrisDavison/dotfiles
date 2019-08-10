let mapleader=" "
" ChrisDavison's vim config

" settings  {{{1
syntax enable
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '-> '
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
set foldenable foldlevelstart=0
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=1
set colorcolumn=0
set hlsearch
set ignorecase smartcase
set tabstop=4 softtabstop=4 shiftround shiftwidth=4 expandtab
set clipboard=unnamed " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw while executing macros
set scrolloff=1
set sidescroll=1
set sidescrolloff=5
set backup
set backupcopy=yes
set backupdir=~/.temp,.
set directory=~/.temp,.
set wildmenu
set wildmode=list:longest,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~
set splitbelow splitright
set laststatus=2
hi User1 guifg=white guibg=purple
set conceallevel=2
set formatoptions+=j  "Delete comment char when joining lines
set history=1000
set tabpagemax=5
set sessionoptions-=options
set viminfo^=!
set t_ut= " Fix issues with background color on some terminals
set relativenumber
set fillchars=fold:·
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'

set path+=**
" }}}1
" statusline {{{1
function! Dir1() " Get only the trailing directory for the statusline
    return fnamemodify(getcwd(), ":t")
endfunction
set statusline=%1*[%l:%c]\ %t\ (%{Dir1()})\ %0*%=%1*\ %Y%R\ %0*
" }}}1
" undo (save undo history across sessions) {{{1
if has('persistent_undo')
    set undodir=~/.undodir
endif
set undofile
" }}} 1
" shell (specialised per os) {{{1
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
elseif executable('/usr/local/bin/zsh')
    set shell=/usr/local/bin/zsh
elseif executable('/usr/local/bin/bash')
    set shell=/usr/local/bin/bash
elseif executable('/bin/bash')
    set shell=/bin/bash
else
    echom "No valid shell!"
endif
" }}}1
" CONDITIONAL SETTINGS / RANDOM STUFF {{{1
if has('path_extra')
    setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if has('nvim')
    set inccommand=nosplit  " Live-preview of :s commands
endif

if !has('nvim') && &ttimeoutlen == -1
    set ttimeout
    set ttimeoutlen=100
endif
" }}}1 Conditional settings
" PLUGINS (3rd party) {{{1
call plug#begin('~/code/dotfiles/vim/3rd_party')
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
Plug 'junegunn/fzf.vim'
Plug 'Konfekt/FastFold'  " More performant fold refreshing
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'dahu/vim-fanfingtastic'  " Let f/F work across line endings
Plug 'easymotion/vim-easymotion'  " Easily navigate to any word or char in buffer
Plug 'ervandew/supertab'
Plug 'kana/vim-textobj-user'  " Custom text objects
Plug 'jceb/vim-textobj-uri'   " Text object for link-type stuff
Plug 'jpalardy/vim-slime'     " Send commands to tmux
Plug 'junegunn/limelight.vim' " De-emphasise paragraphs around your current one
Plug 'junegunn/goyo.vim'      " 'Focus' mode (centered text buffer)
Plug 'romainl/vim-qlist'
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'   " Comment modification/text objects
Plug 'tpope/vim-fugitive'     " Git integration
Plug 'tpope/vim-surround'     " 'Surround' text objects e.g. csi(
Plug 'tpope/vim-unimpaired'   " Deal with bracket/surrounding pairs
Plug 'tpope/vim-eunuch'       " More integrated unix commands (mv, rm etc)
Plug 'tpope/vim-vinegar'      " Easily navigate directories
Plug 'wellle/targets.vim'
Plug 'junegunn/seoul256.vim'  " Seoul256 theme
Plug 'natebosch/vim-lsc'
Plug 'kshenoy/vim-signature'
Plug 'ludovicchabant/vim-gutentags'
Plug 'skywind3000/gutentags_plus'
Plug 'tomasr/molokai'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
" Plug 'fmoralesc/vim-extended-autochdir'
call plug#end()
" }}}1
" keybinds {{{1
" =====[ Edit files and source vimrc ]=====
" These versions are for when I don't have fzf and fzf.vim installed
" nnoremap <leader>ev :edit ~/code/dotfiles/.vimrc<CR>
" nnoremap <leader>en :edit ~/Dropbox/notes/**/*
" nnoremap <leader>b :ls<Cr>:b
" nnoremap <leader>s  :ls<CR>:filt  ls<LEFT><LEFT><LEFT>
" nnoremap <leader>p :find

nnoremap <leader>en :Files ~/Dropbox/notes/<CR>
nnoremap <leader>es :Files ~/code/scripts/<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>p :Files<CR>

" =====[ Uppercase the current word (from anywhere within the <word>) ]=====
inoremap <C-u>   <esc>mzgUiw`za

nnoremap <silent> Q =ip

" =====[ Generic useful stuff ]=====
inoremap jj     <ESC>:w<CR>
nnoremap <BS>   <C-^>
nnoremap S      :%s///<LEFT>
vnoremap S      :s///<LEFT>
vnoremap <      <gv
vnoremap >      >gv
nnoremap j      gj
nnoremap k      gk

nnoremap K :silent! lgrep! "\b<C-R><C-W>\b"<CR>:lw<CR>
nnoremap <leader>g :silent! lgrep! ""<LEFT>
" }}}1
" random autocommands {{{1
" file-specific are in DOTFILES/vim/ftplugin/<lang>.vim
augroup vimrc
    autocmd!
    au ColorScheme * hi! link SignColumn LineNr
    au TextChanged,InsertLeave,FocusLost * silent! wall
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au User GoyoEnter Limelight | exec "normal zz" | Typewrite
    au User GoyoLeave Limelight! | Typewrite!
    au BufWritePost $MYVIMRC source $MYVIMRC
augroup END
" }}}1
" strip trailing whitespace {{{1
function! StripTrailingWhitespace()
    if !&binary && &filetype != 'diff'
        if getpos("'z")[1] != getpos(".")[1]
            normal mz
        endif
        %s/\s\+$//e
        if getpos("'z")[2] != 0
            normal `z
            delm z
        endif
    endif
endfunction
" }}}1
" toggle concealling {{{1
function! ToggleConceal()
    if &conceallevel == 2
        set conceallevel=0
    else
        set conceallevel=2
    endif
endfunction
nnoremap <C-y>      :call ToggleConceal()<CR>
" }}}1
" C / Cpp / Arduino {{{1
augroup c_cpp_arduino
    autocmd!
    au Filetype arduino set filetype=cpp
    au Filetype c,cpp set foldmethod=syntax
augroup END
" }}}1
" Golang {{{1
augroup golang
    autocmd!
    au Filetype go set foldmethod=syntax
augroup END
let g:go_fmt_command="goimports"
" }}}1
" javascript {{{1
let b:javascript_fold=1
augroup javascript
    autocmd!
    au Filetype javascript setlocal foldmethod=syntax
augroup END
" }}}1
" make {{{1
augroup make
    autocmd!
    au Filetype make setlocal noexpandtab
augroup END
" }}}1
" markdown/pandoc {{{1
function! VimNewMarkdown(fname) abort
    exec ":normal 0i# " . substitute(fnamemodify(a:fname, ':t:r:gs/-/ /'), "\\<.", "\\u&", "g")
endfunction
augroup markdown
    autocmd!
    au BufNewFile *.md exec VimNewMarkdown(expand("<afile>"))
    au BufWritePre *.md call StripTrailingWhitespace()
    au BufRead,BufNewFile *.md set filetype=pandoc
    au Filetype pandoc setlocal tw=80
    au Filetype pandoc setlocal foldmethod=expr
    au Filetype pandoc setlocal equalprg=pandoc\ --to\ markdown-shortcut_reference_links+pipe_tables-simple_tables\ --columns=80\ --reference-links\ --reference-location=section\ --atx-headers
    au Filetype pandoc setlocal nospell 
    au Filetype pandoc nnoremap D dip
augroup END
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'rust', 'go', 'c', 'cpp']

let g:pandoc#folding#fdc=0
let g:pandoc#formatting#mode="hA"
let g:pandoc#formatting#textwidth=80
let g:pandoc#spell#enabled=0
let g:pandoc#hypertext#autosave_on_edit_open_link=1
let g:pandoc#hypertext#create_if_no_alternates_exists=1
let g:pandoc#formatting#smart_autoformat_on_cursormoved=0
let g:pandoc#formatting#equalprg="pandoc --to markdown-shortcut_reference_links+pipe_tables-simple_tables --columns=81"
let g:pandoc#formatting#extra_equalprg="--reference-links --reference-location=section --atx-headers"
let g:pandoc#syntax#style#use_definition_lists=0
let g:pandoc#syntax#conceal#use=0
let g:pandoc#syntax#conceal#blacklist=['subscript', 'superscript', 'list', 'atx', 'ellipses', 'codeblock_start', 'codeblock_delim']
let g:pandoc#toc#close_after_navigating=0
let g:pandoc#syntax#conceal#use=1

nnoremap <silent><buffer> <C-n>      :ThesisNotes<CR>

iabbrev CITE ^[cite -]<LEFT>

if exists('g:loaded_toggleconceal')
    call ToggleConceal()
    call ToggleConceal()
endif
" }}}1
" python {{{1
augroup python
    autocmd!
    au Filetype python setlocal foldmethod=indent
augroup END
let g:pymode_python = 'python3'
let g:slime_paste_file=tempname()
let g:slime_python_ipython = 1
let g:slime_target = "tmux"
if !has('win32')
    let g:lsc_server_commands = {'python': 'pyls'}
endif
" }}}1
" rust {{{1
augroup rust
    autocmd!
    au Filetype rust setlocal foldmethod=syntax
augroup END
let g:rustfmt_autosave=1
"if executable('rls')
"    au User lsp_setup call lsp#register_server({
"                \ 'name': 'rls',
"                \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
"                \ 'whitelist': ['rust'],
"                \})
"endif
" }}}1
" bash / shellscript {{{1
augroup shellscript
    autocmd!
    au Filetype sh setlocal foldmethod=syntax 
augroup END
let g:sh_fold_enabled=5
let g:is_bash=1
" }}}1
" tex / latex {{{1
augroup tex_latex
    autocmd!
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex setlocal tw=80
    au Filetype tex setlocal colorcolumn=80
    au Filetype tex setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
augroup END
let g:tex_flavor = "latex"
let g:vimtex_fold_enabled=1
let g:vimtex_compiler_progname='nvr'

nnoremap <buffer> <C-n>      :ThesisNotes<CR>
" }}}1
" vimscript {{{1
augroup vimscript
    autocmd!
    au Filetype vim setlocal foldmethod=marker
augroup END
" }}}1
" abbreviations {{{1
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
cnoreabbrev Q! q!

iabbrev meanstd μ±σ
" }}}1
" custom commands {{{1
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
command! Scratch edit ~/.scratch | normal G
command! NOH silent! /aksjdkajsd<CR>
command! CD exec "cd ".expand("%:h")
command! RMD exec "!rm ".expand("%") | bp | bd #
command! Notes edit ~/Dropbox/notes/notes.md | normal G
command! Logbook exec "e " . expand(strftime("~/Dropbox/notes/logbook/%Y/%m/%Y-%m-%d.md")) | normal G
" }}}1
" typewriter mode {{{1
let g:typewriter_mode_active=0
function! s:ToggleTypewriting(bang)
    if a:bang != "!"
        nnoremap <silent> j @='jzz'<CR>
        nnoremap <silent> k @='kzz'<CR>
        vnoremap <silent> j @='jzz'<CR>
        vnoremap <silent> k @='kzz'<CR>
        let g:typewriter_mode_active=1
    else
        nnoremap <silent> j j
        nnoremap <silent> k k
        vnoremap <silent> j j
        vnoremap <silent> k k
        let g:typewriter_mode_active=0
    endif
endfunction
command! -bang Typewrite call <SID>ToggleTypewriting("<bang>")
" }}}1
" org-mode like scheduling/todo keywords {{{1
let g:schedule_words = [ 'TODO' , 'WAITING', 'DONE', 'CANCELLED' ]
function! RotateWord()
    let N = len(g:schedule_words)
    for word in g:schedule_words
        let coln = match(getline(line(".")), word)
        if coln > 0
            let idx = index(g:schedule_words, word)
            let next = g:schedule_words[(idx+1) % N]
                        call cursor(0, coln+1)
            exec "normal ciw" . next 
            exec "normal $"
            return
        endif
    endfor
endfunction
command! RotateScheduleWord call RotateWord()
    nnoremap <M-r>      :RotateScheduleWord<Cr>
    inoremap <M-r>      <C-o>:RotateScheduleWord<Cr>
" }}}1
" help in tabs (DISABLED) {{{1
"function! ToggleHelpInTabs()
"    let g:help_in_tabs = !g:help_in_tabs
"    echo "Help in tabs? ".g:help_in_tabs
"endfunction 

""Only apply to .txt files...
"augroup HelpInTabs
"    autocmd!
"    autocmd BufEnter  *.txt   call HelpInNewTab()
"augroup END

""Only apply to help files...
"function! HelpInNewTab ()
"    if &buftype == 'help' && g:help_in_tabs
"        "Convert the help window to a tab...
"        execute "normal \<C-W>T"
"    endif
"endfunction

""Simulate a regular cmap, but only if the expansion starts at column 1...
"function! CommandExpandAtCol1 (from, to)
"    if strlen(getcmdline()) || getcmdtype() != ':'
"        return a:from
"    else
"        return a:to
"    endif
"endfunction

""Expand hh -> helpg...
"cmap <expr> hh CommandExpandAtCol1('hh','helpg ')
" }}}1
" templates / skeletons for files {{{1
function! ReadFileTemplate()
    let ext = expand("%:e")
    let dir = globpath(&rtp, "file_templates")
    if isdirectory(dir)
        let fname = dir . "/template." . ext
        if filereadable(fname)
            exec "read " . fname
            normal ggdd
        endif
    else
        echom "ReadFileTemplate: Couldn't find template dir"
        return
    endif
endfunction

function! ReadTemplate(fname)
    normal mA
    let dir = globpath(&rtp, "file_templates")
    let fname = dir . '/' . a:fname
    if filereadable(fname)
        normal dd
        exec "read " . fname
    else
        echom "ReadTemplate: Couldn't find file"
        normal `AmA
        return
    endif
endfunction

command! -nargs=1 ReadTemplate call ReadTemplate(<f-args>)
" }}}1
" make nonexistent directories on write {{{1
function! MakeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction

augroup nonExDir
    autocmd!
    autocmd BufWritePre * call MakeNonExDir()
augroup END
" }}}1
" grep / ripgrep {{{1
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case

    command! -bang -nargs=* Rg
                \ call fzf#vim#grep(
                \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1, 
                \ fzf#vim#with_preview('right:50%:hidden', '?'),
                \ <bang>0)
endif

" CTRL-A CTRL-Q to select all and build quickfix list
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
" }}}1
" toggle colour column {{{1
function! ToggleColorcolumn()
    if &colorcolumn > 0
        set colorcolumn=0
    else
        set colorcolumn=80
    endif
endfunction
command! ToggleColorColumn call ToggleColorcolumn()
" }}}1
" appearance {{{1
if has('gui')
    if has('win32')
        set gfn=Fantasque_Sans_Mono:h14
    else
        set gfn=FantasqueSansMono-Regular:h24
    endif
endif

set t_Co=256
set bg=dark
" when do I need termguicolours? why did I switch it off?
" problem between vim and neovim? terminal and gui? windows vs osx?
" set termguicolors
silent! colorscheme seoul256
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L
" }}}1
" highlight interesting words {{{1
" FOUND ONLINE SOMEWHERE
"
" This mini-plugin provides a few mappings for highlighting words temporarily.
"
" Sometimes you're looking at a hairy piece of code and would like a certain
" word or two to stand out temporarily.  You can search for it, but that only
" gives you one color of highlighting.  Now you can use <leader>N where N is
" a number from 1-6 to highlight the current word in a specific color.

function! HiInterestingWord(n)
    " Save our location.
    normal! mz
    " Yank the current word into the z register.
    normal! "zyiw
    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n
    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)
    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'
    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)
    " Move back to our original location.
    normal! `z
endfunction

" Mappings
nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>

function! s:ClearMatches()
    silent! call matchdelete(86751)
    silent! call matchdelete(86752)
    silent! call matchdelete(86753)
    silent! call matchdelete(86754)
    silent! call matchdelete(86755)
    silent! call matchdelete(86756)
endfunction
command! NoWord silent! call <SID>ClearMatches()
"
" Default Highlights

hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195
" }}}1
" RANDOM STUFF TO TIDY {{{1
" =====[ Config for downloaded plugins ]=====
let g:SuperTabDefaultCompletionType = "context"

if executable('rg')
    set grepprg=rg\ --vimgrep
endif
" }}}1

cd ~/Dropbox/notes
