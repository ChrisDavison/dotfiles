" Split/Window management {{{
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

" }}}

" Move by VISUAL lines {{{
nnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
vnoremap  <buffer> <silent> k gk
nnoremap  <buffer> <silent> j gj
vnoremap  <buffer> <silent> j gj
nnoremap  <buffer> <silent> 0 g0
vnoremap  <buffer> <silent> 0 g0
nnoremap  <buffer> <silent> $ g$
vnoremap  <buffer> <silent> $ g$
" }}}

" Quicker search/replace {{{
" Basically, put you between the brackets of s//g,
" type your search, then /, then your replacement
nmap S :%s//g<LEFT><LEFT>
vmap S :s//g<LEFT><LEFT>
" }}}

" Selecting and Pasting {{{
" Paste and move to end
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Select what was pasted
noremap gV `[v`]

" Select ALL
nnoremap <leader>a ggVG
" }}}

" Navigate quickfix/locationlist with keychords {{{
" if empty(getloclist(0))
"     nnoremap <C-S-n> :cn<CR>
"     nnoremap <C-S-p> :cp<CR>
" else
"     nnoremap <C-S-n> :lnext<CR>
"     nnoremap <C-S-p> :lprev<CR>
" endif
" }}}

" Buffer/File/Function/Outline navigation (CtrlP versus FZF) {{{

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
" }}}

" Keybinds to manipulate my vim config {{{
nnoremap <leader>ev :e $MYVIMRC<Cr>
nnoremap <leader>sv :so $MYVIMRC<Cr>

nnoremap <leader>et :e ~/.vim/vimrc/totidy.vim<Cr>Go
nnoremap <leader>V :e ~/.vim/vimrc/
" }}}

" Keybinds to go to specific files/dirs {{{
nnoremap <leader>es :e $SRCME/
nnoremap <leader>ee :e $HOME/dev/etc/
nnoremap <leader>ei :e $HOME/Dropbox/notes/inbox.md<Cr>
" }}}

" Miscellany {{{
" Toggle hidden character visibility with
nmap <Leader>h :set list!<CR>
set listchars=tab:▸\ ,trail:·,extends:❯,precedes:❮,nbsp:×,eol:¬

" Toggle Vimtex Table of Contents
" nnoremap <leader>v :VimtexTocToggle<CR>

" Toggle tagbar
nnoremap <leader>t :TagbarToggle<CR>

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

nnoremap mp :call MDPreview()<Cr>

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
