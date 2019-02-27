" ChrisDavison's vim config
let mapleader=" "
" settings (using tpope/vim-sensible as a base) {{{
syntax on
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '↳ '
set cpo+=n
set breakindent
set breakindentopt+=shift:2,sbr
set number norelativenumber
set iskeyword=a-z,A-Z,_,.,39  " Used e.g. when searching for tags
set tags=./tags;,tags,.git/tags
set hidden
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
else
    if executable('/usr/local/bin/fish')
        set shell=/usr/local/bin/fish
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
" utility
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'airblade/vim-gitgutter'
Plug 'godlygeek/tabular'
Plug 'Konfekt/FastFold'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'dahu/vim-fanfingtastic'
Plug 'dhruvasagar/vim-table-mode'
Plug 'easymotion/vim-easymotion'
Plug 'ervandew/supertab'
Plug 'kana/vim-textobj-user'
Plug 'jceb/vim-textobj-uri'
Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/goyo.vim'
Plug 'romainl/vim-qlist'
Plug 'romainl/vim-qf'
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-vinegar'
Plug 'wellle/targets.vim'
Plug 'itchyny/lightline.vim'
" themes
" FLAZZ is a massive colorscheme pack
Plug 'junegunn/seoul256.vim'
call plug#end()
" }}}
" appearance {{{
set t_Co=256
set bg=dark
silent! colorscheme seoul256
let g:lightline = { 'colorscheme': 'seoul256' }
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
nnoremap <leader>r :redraw<CR>
nnoremap <leader>= gqap
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
" custom commands {{{
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
command! ASMR edit ~/Dropbox/asmr.csv | normal G
command! Journal edit ~/Dropbox/notes/journal.md | normal G
command! Todos edit ~/Dropbox/notes/todo.md | normal G
command! Dones edit ~/Dropbox/notes/done.md | normal G
command! Projects edit ~/Dropbox/notes/projects.md | normal G
command! NOH :silent! /ajsdkajskdj<CR>
" }}}
" autocommands {{{
augroup vimrc
    autocmd!
    autocmd FileType c,cpp,arduino,go,rust,javascript set foldmethod=syntax
    autocmd FileType python  set foldmethod=indent
	autocmd BufNewFile,BufWinEnter *.md set filetype=markdown
    autocmd BufWritePre *.md,*.txt,*.csv %s/\s\+$//e
    autocmd BufNewFile *.md exec VimNewMarkdown(expand("<afile>"))
    autocmd Filetype markdown setlocal foldexpr=MarkdownLevel() foldmethod=expr
    autocmd Filetype markdown setlocal tw=80 autoindent
    autocmd Filetype markdown setlocal colorcolumn=80
    autocmd Filetype markdown hi Conceal cterm=NONE ctermbg=NONE
    autocmd Filetype markdown hi Conceal guibg=NONE guifg=NONE
    autocmd BufWinEnter todo.md highlight TodoDate ctermfg=red
    autocmd BufWinEnter todo.md match TodoDate /\d\d\d\d-\d\d-\d\d/
    autocmd FileType make    set noexpandtab
    autocmd FileType vim     set foldmethod=marker
    autocmd ColorScheme * hi! link SignColumn LineNr
    autocmd BufNewFile,BufReadPost *.tex set filetype=tex
    autocmd TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd CursorHold * silent! checktime " Check for external changes to files
    autocmd VimResized * wincmd= " equally resize splits on window resize
    autocmd FileType sh let g:sh_fold_enabled=5
    autocmd FileType sh let g:is_bash=1
    autocmd FileType sh set foldmethod=syntax
    autocmd User GoyoEnter Limelight
    autocmd User GoyoLeave Limelight!
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
" }}}
" custom folding for markdown headers {{{
function! MarkdownLevel()
    let h = matchstr(getline(v:lnum), '^#\+')
    if empty(h)
        return "="
    endif
    return ">" . len(h)
endfunction
" }}}
" custom fold text {{{
function! NeatFoldText()
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '[' . printf("%10s", lines_count . ' lines') . ']'
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextstart = strpart('+' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction
set foldtext=NeatFoldText()
" }}}
" FZF && Rg/Ag {{{
if executable('rg')
    set grepprg=rg\ --vimgrep
    " let s:find_cmd=
    command! -bang -nargs=* Find call fzf#vim#grep(
    \    'rg --no-heading -F --smart-case --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
    nnoremap <leader>F :Find<SPACE>
endif
" }}}
" custom functions {{{
" Strip trailing whitespace {{{2
function! StripTrailingWhitespace()
  if !&binary && &filetype != 'diff'
    normal mz
    normal Hmy
    %s/\s\+$//e
    normal 'yz<CR>
    normal `z
  endif
endfunction
command! StripWhitespace exec StripTrailingWhitespace()
"}}}2
" Insert filename as header of new markdown file {{{2
function! VimNewMarkdown(fname)
    exec ":normal 0i# " . substitute(fnamemodify(a:fname, ':t:r:gs/-/ /'), "\\<.", "\\u&", "g")
endfunction
"}}}2
" Open current logbook entry {{{2
function! CurrentLogbook()
    let logbooks=globpath(expand("~/Dropbox/notes/work/logbook"), "*.md", 0, 1)
    let last_logbook=get(logbooks, len(logbooks)-1)
    exec ":e ".last_logbook | normal G
endfunction
command! Logbook exec CurrentLogbook()
"}}}2
" Navigate between thesis and notes {{{2
function! ThesisNotes()
    if match(expand("%:p"), "thesis") >= 0
        if expand("%:p:h:t") == "notes"
            exec ":e ../" . expand("%:n") | normal `z
        else
			exec ":normal mz"
            exec ":e notes/" . expand("%:n")
        endif
    endif
endfunction
command! ThesisNotes exec ThesisNotes()
nnoremap <silent> <leader>tn :ThesisNotes<CR>
" }}}2
" Toggle concealing {{{2
set conceallevel=2
function! ToggleConceal()
    if &conceallevel == 2
        set conceallevel=0
    else
        set conceallevel=2
    endif
endfunction
nnoremap <silent> <C-y> :call ToggleConceal()<CR>

" }}}2
" Toggle color column {{{2
function! s:ToggleColorcolumn()
    if &colorcolumn > 0
        set colorcolumn=0
    else
        set colorcolumn=80
    endif
endfunction
command! ToggleColorColumn call s:ToggleColorcolumn()

" }}}2
"}}}
