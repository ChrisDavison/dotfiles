" chrisDavison's vim config
let mapleader="\\"
" settings {{{
" If 'set' commands appear missing, it's because I'm using tpope/vim-sensible
execute pathogen#infect()
syntax on
filetype plugin indent on

set nocompatible " Don't force compability with vi
set autochdir    " cd to the directory of the currently edited file
set wrap lbr
let &showbreak = '└ '
set omnifunc=syntaxcomplete#Complete
set number " Line numbers
set iskeyword=a-z,A-Z,_,.,39
set hidden
let haswin=has('win32') || has('win64')
if haswin
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
set foldenable
set foldlevelstart=0
set updatetime=1000 " Write a swap file after 1 second
set cmdheight=2  " Useful for more info on some plugins

" --- Search options
set gdefault " By default, replace all matches on a line (i.e. always s///g)
set hlsearch " Highlight search results
set ignorecase
set smartcase

" --- Various coding preferences
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab " Convert tabs to spaces
set clipboard=unnamed " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw _while_ exeecuting macros
set title         " Show filename as window title
set sidescroll=1

" --- Put all temp files in one place
set backup
set backupcopy=yes
set backupdir=~/.temp,.
set directory=~/.temp,.

" --- Wildmenu config
set wildmode=list:longest,full
set wildignore+=*DS_Store*
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

set statusline=%<\ %t\ %=%(%l/%L\|%c%),\ %{exists('g:loaded_fugitive')?FugitiveStatusCD():''}\ %Y\ 

" --- Miscellany
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'
if has('persistent_undo')
    set undodir=~/.undodir/
    set undofile
    endif
set t_ut= " Fix issues with background color on some terminals

" FZF && Rg/Ag {{{2
if executable('rg')
    set grepprg=rg\ --vimgrep
    " let s:find_cmd=
    command! -bang -nargs=* Find call fzf#vim#grep(
    \    'rg --column  --no-heading -F --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
    nnoremap <leader>F :Find<SPACE>
endif
" }}}2
" }}}
" appearance {{{
set t_Co=256
set bg=dark
silent! colorscheme seoul256
if has('gui_running')
    set guioptions-=l
    set guioptions-=L
    set guioptions-=r
    set guioptions-=R
    set guioptions-=T
    set guioptions-=m
    set guifont=Iosevka:h14
    if haswin
        set guifont=Fantasque\ Sans\ Mono:h14
    endif
endif
" }}}
" keybinds {{{
" command abbreviatons {{{2
cnoreabbrev W w
cnoreabbrev Qa qa
" }}}2
" move by visual lines {{{2
vmap  <buffer> <silent> k gk
vmap  <buffer> <silent> j gj
vmap  <buffer> <silent> 0 g0
vmap  <buffer> <silent> $ g$
" }}}2
" buffer/file/function/outline navigation using FZF {{{2
nnoremap <leader>bb :Buffers<Cr>
nnoremap <leader>p :Files<Cr>
nnoremap <leader>ll :Lines<cr>
nnoremap <leader>lb :BLines<cr>
nnoremap <leader>m :Marks<cr>
" }}}2
" modify/source my vimrc {{{2
nnoremap <leader>ev :e $MYVIMRC<Cr>G
nnoremap <leader>sv :so $MYVIMRC<Cr>
" }}}2
" backspace goes to `alternate` file {{{2
nnoremap <BS> <C-^>
" }}}2
" easily search/replace using last search {{{2
nmap S :%s///<LEFT>
vnoremap S :s///<LEFT>
" }}}2
" toggle 'conceal' mode {{{2
function! ToggleConceal()
    if &conceallevel == 2
        set conceallevel=0
    else
        set conceallevel=2
    endif
endfunction
command! ToggleConceal call ToggleConceal()
nnoremap <silent> <C-y> :ToggleConceal<CR>
" }}}2
" indent/de-dent visual selection {{{2
vnoremap < <gv
vnoremap > >gv
" }}}2
" uncategorised bindings {{{2
nnoremap <leader>t :Tags<CR>
nnoremap <leader>bt :BTags<CR>
nnoremap <Leader>h :set list!<CR>
nnoremap nw :set wrap!<CR>
" }}}2
" fold with space {{{2
noremap <space> :normal zA<CR>
" }}}2
" }}}
" plugins / languages {{{
" autocommands {{{2
augroup vimrc
    autocmd!
    autocmd FileType c set foldmethod=syntax
    autocmd Filetype cpo set foldmethod=syntax
    autocmd Filetype arduino set foldmethod=syntax
    autocmd FileType python  set foldmethod=indent
    autocmd FileType python  set tabstop=4
    autocmd FileType python  set softtabstop=4
    autocmd FileType python  set iskeyword=a-z,A-Z,_
    autocmd FileType python  nnoremap <LocalLeader>i :!isort %<cr><cr>
    autocmd FileType python  nnoremap <LocalLeader>= :0,$!yapf<CR>
    autocmd FileType go      set foldmethod=syntax
    autocmd Filetype markdown set conceallevel=0
    autocmd Filetype markdown setlocal foldexpr=MarkdownLevel()
    autocmd Filetype markdown setlocal foldmethod=expr
    autocmd Filetype markdown hi Conceal cterm=NONE ctermbg=NONE
    autocmd Filetype markdown hi Conceal guibg=NONE guifg=NONE
    autocmd Filetype markdown set textwidth=80
    autocmd Filetype markdown set formatoptions+=t
    autocmd BufReadPost *.md setlocal foldmethod=expr
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
    autocmd TextChanged,InsertLeave,FocusLost * silent! wall " Write files on focus lost
    autocmd CursorHold * silent! checktime " Check for external changes to files
    autocmd VimResized * wincmd= " equally resize splits on window resize
	autocmd BufWinEnter *.py,*.go,*.rs,*.cpp,*.c,*.js let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
augroup END
" }}}2
" specific language config {{{2
" python
let g:pymode_python = 'python3'
let g:slime_target = "tmux"
let g:slime_python_ipython = 1
let g:slime_paste_file=tempname()
" go
let g:go_fmt_command = "goimports"
" markdown
let g:vim_markdown_folding_disabled = 0
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_follow_anchor = 1
" latex
let g:tex_flavor = "latex"
let g:vimtex_quickfix_ignore_all_warnings=1
let g:vimtex_latexmk_continuous=0
let g:vimtex_quickfix_mode=0
let g:vimtex_indent_enabled=1
let g:vimtex_fold_enabled=1
" tables
let g:table_mode_corner="|"
let g:table_mode_corner_corner="|"
let g:table_mode_header_fillchar="-"
" rust
let g:racer_cmd="/Users/davison/.cargo/bin/racer"
if haswin
    let g:racer_cmd="c:\\Users\\user01\\.cargo\\bin\\racer.exe"
endif
let g:racer_experimental_completer=1
let g:echodoc_enable_at_startup=1
if executable('rls')
    au User lsp_setup call lsp#register_server({
                \ 'name': 'rls',
                \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
                \ 'whitelist': ['rust'],
                \})
endif
" extra
let b:javascript_fold=1
let g:SuperTabDefaultCompletionType = "context"
let perl_fold = 1
" gitgutter
let g:gitgutter_sign_added = '∙'
let g:gitgutter_sign_modified = '∙'
let g:gitgutter_sign_removed = '∙'
let g:gitgutter_sign_modified_removed = '∙'
if haswin
    let gitgutter_enabled=0
endif
" linting
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter=0
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
command! -bar -nargs=? -bang SScratch :silent new<bang>|set buftype=nofile bufhidden=hide noswapfile buflisted filetype=<args> modifiable
nnoremap <silent>  == :Scratch<CR>
nnoremap <silent>  =" :Scratch<Bar>put<Bar>1delete _<Bar>filetype detect<CR>
nnoremap <silent>  =* :Scratch<Bar>put *<Bar>1delete _<Bar>filetype detect<CR>
nnoremap <silent>  =p :SScratch<Bar>put *<Bar>1delete _<Bar>filetype detect<CR>
nnoremap           =f :Scratch<Bar>set filetype=
" }}}
" miscellaneous/experimental {{{
command! CopyFilename exec "@+=expand(\"%\")"
command! CopyRelativeFilename exec "@+=expand(\"%:p\")"
command! Wd write|bdelete
command! Bd bp|bd #
if haswin
    cd ~
end
" }}}
