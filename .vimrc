" chrisDavison's vim config
let mapleader=" "
" settings (using tpope/vim-sensible as a base) {{{
execute pathogen#infect()
syntax on
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '└ '
set number relativenumber
set iskeyword=a-z,A-Z,_,.,39  " Used e.g. when searching for tags
set hidden
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
else
    if executable('/usr/local/bin/zsh')
        set shell=/usr/local/bin/zsh
    else
        set shell=/bin/zsh
    endif
endif
set nospell
set foldenable foldlevelstart=0
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=2  " Useful for more info on some plugins
set colorcolumn=100 " Put a bar at column 100
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
if has('persistent_undo')
    set undodir=~/.undodir/ undofile
endif
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'
" }}}
" appearance {{{
set t_Co=256
set bg=dark
silent! colorscheme gruvbox
if has('gui_running')
    set guioptions=
    set guifont=Iosevka:h16
    if has('win32')
        set guifont=Hack:h14
    endif
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
" indent/de-dent visual selection
vnoremap < <gv
vnoremap > >gv
" Mostly stuff from FZF for navigating buffers
nnoremap <leader>b :Buffers<Cr>
nnoremap <leader>p :Files<Cr>
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
function! ToggleConceal()
    if &conceallevel == 2
        set conceallevel=0
    else
        set conceallevel=2
    endif
endfunction
nnoremap <silent> <C-y> :call ToggleConceal()<CR>
" }}}
" plugins / languages {{{
augroup vimrc
    autocmd!
    autocmd FileType c,cpp,arduino,go,rust,javascript set foldmethod=syntax
    autocmd FileType python  set foldmethod=indent
	autocmd BufNewFile,BufWinEnter *.md set filetype=markdown
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
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter=1
let g:ale_set_quickfix=1
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
endif
" }}}2
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
" Handling logbook
function! s:last_logbook()
    let files=globpath('$LOGBOOK_DIR', '20*.md')
    let fn=split(files, '\n')[-1]
    exec "edit ".fn
endfunction
function! s:logbook_today()
    let t=localtime()
    let mydir=$LOGBOOK_DIR
    let fn=mydir."/".strftime("%Y-%m-%d--%a.md", t)
    exec "edit ".fn
endfunction
command! LBprevious call s:last_logbook()
command! LBtoday call s:logbook_today()
" }}}
cd ~/devel
