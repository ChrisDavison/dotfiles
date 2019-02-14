" ChrisDavison's vim config
let mapleader=" "
" settings (using tpope/vim-sensible as a base) {{{
syntax on
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '└ '
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
set foldenable foldlevelstart=0
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=2  " Useful for more info on some plugins
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
Plug 'aliev/vim-compiler-python'
Plug 'dag/vim-fish'
Plug 'elixir-editors/vim-elixir'
Plug 'fatih/vim-go'
Plug 'guns/vim-clojure-static'
Plug 'lervag/vimtex'
Plug 'neovimhaskell/haskell-vim'
Plug 'plasticboy/vim-markdown'
Plug 'rust-lang/rust.vim'
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'vim-jp/vim-cpp'
Plug 'vim-perl/vim-perl'
Plug 'vim-python/python-syntax'
Plug 'vim-scripts/gnuplot-syntax-highlighting'
Plug 'vimjas/vim-python-pep8-indent'
Plug 'zah/nim.vim'
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
Plug 'tpope/vim-vinegar'
Plug 'wellle/targets.vim'
Plug 'itchyny/lightline.vim'
" themes
Plug 'junegunn/seoul256.vim'
Plug 'Lokaltog/vim-monotone'
call plug#end()
" }}}
" appearance {{{
set t_Co=256
set bg=dark
let &termguicolors = 1
" let g:monotone_color = [268, 100, 89]
" let g:monotone_secondary_hue_offset = 200
let g:monotone_emphasize_comments = 1
silent! colorscheme monotone
if has('gui_running')
    set guioptions=
    set guifont=Hack:h16
endif
" }}}
" keybinds {{{
" command abbreviatons
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
" move by visual lines
vmap  <buffer> <silent> k gk
vmap  <buffer> <silent> j gj
vmap  <buffer> <silent> 0 g0
vmap  <buffer> <silent> $ g$
nmap  <buffer> <silent> k gk
nmap  <buffer> <silent> j gj
nmap  <buffer> <silent> 0 g0
nmap  <buffer> <silent> $ g$
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
" easily search/replace using last search
nmap S :%s///<LEFT>
vnoremap S :s///<LEFT>
" Other bindings
nnoremap <leader>ev :e $MYVIMRC<BAR>echo "Editing VIMRC"<CR>
nnoremap <leader>sv :so $MYVIMRC<BAR>echo "Sourced VIMRC"<CR>
nnoremap <Leader>hh :set list!<BAR>echo "Toggle hidden characters"<CR>
nnoremap nw :set wrap!<BAR>echo "Toggling line wrapping"<CR>
nnoremap <BS> <C-^>
" toggle 'conceal' mode
set conceallevel=2
function! ToggleConceal()
    if &conceallevel == 2
        set conceallevel=0
    else
        set conceallevel=2
    endif
endfunction
nnoremap <silent> <C-y> :call ToggleConceal()<CR>
function! s:ToggleColorcolumn()
    if &colorcolumn > 0
        set colorcolumn=0
    else
        set colorcolumn=100
    endif
endfunction
command! ToggleColorColumn call s:ToggleColorcolumn()
" }}}
" plugins / languages {{{
augroup vimrc
    autocmd!
    autocmd FileType c,cpp,arduino,go,rust,javascript set foldmethod=syntax
    autocmd FileType python  set foldmethod=indent
	autocmd BufNewFile,BufWinEnter *.md set filetype=markdown
    autocmd BufNewFile * -1r !vim_file_template <afile>
    autocmd BufNewFile * :silent call search('^.*implementation here')
    autocmd BufNewFile * :redraw
    autocmd BufWritePre *.md,*.txt,*.csv %s/\s\+$//e
    autocmd Filetype markdown setlocal foldexpr=MarkdownLevel()
    autocmd Filetype markdown setlocal foldmethod=expr
    autocmd Filetype markdown hi Conceal cterm=NONE ctermbg=NONE
    autocmd Filetype markdown hi Conceal guibg=NONE guifg=NONE
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
" specific language config {{{2
let g:pymode_python = 'python3'
let g:slime_target = "tmux"
let g:slime_python_ipython = 1
let g:slime_paste_file=tempname()
let g:go_fmt_command = "goimports"
let g:tex_flavor = "latex"
if executable('rls')
    au User lsp_setup call lsp#register_server({
                \ 'name': 'rls',
                \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
                \ 'whitelist': ['rust'],
                \})
endif
let b:javascript_fold=1
let g:SuperTabDefaultCompletionType = "context"
" }}}2
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
  let lines_count_text = '-- ' . printf("%10s", lines_count . ' lines') . ' '
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextstart = strpart('+' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction
set foldtext=NeatFoldText()
" }}}
" scratch buffers {{{
command! -bar -nargs=? -bang Scratch :silent enew<bang>|set buftype=nofile bufhidden=hide noswapfile buflisted filetype=<args> modifiable
nnoremap <silent>  == :Scratch<CR>
nnoremap           =f :Scratch<Bar>set filetype=
" }}}
" miscellaneous/experimental {{{
" FZF && Rg/Ag {{{2
if executable('rg')
    set grepprg=rg\ --vimgrep
    " let s:find_cmd=
    command! -bang -nargs=* Find call fzf#vim#grep(
    \    'rg --column  --no-heading -F --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
    nnoremap <leader>F :Find<SPACE>
    command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   <bang>0 ? fzf#vim#with_preview('up:60%')
      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0)
endif
" }}}2
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
command! ASMR edit ~/Dropbox/asmr.csv | normal G
command! Journal edit ~/Dropbox/notes/journal.md | normal G
command! Todos edit ~/Dropbox/notes/todos.md | normal G
command! Dones edit ~/Dropbox/notes/dones.md | normal G

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

let g:rustfmt_autosave = 1
let g:vimtex_toc_config = {
            \'fold_enable': 1,
            \'split_pos': 'rightbelow'}

command! ILH :normal [I<CR> | Keep expand('%')<CR>
" }}}
