" vim: fdm=marker
let mapleader=" "

" Load plugins (and docs on my .vim/plugin) {{{1
execute pathogen#infect("~/.vim/bundle/{}")

" Some plugins and autocommands have been split out
" .vim/plugin/...
"     headerjump.vim
"     markdown_foldlevel.vim
"     markdown_backlinks.vim
"     markdown_gotofile.vim
"     make_nonexistent_dir.vim
"     file_from_selected_text.vim
"     get_visual.vim
"     sanitise_filename.vim
"     window_width.vim

" .vim/after/ftplugin/...
"     markdown.vim
"     tex.vim

" .vim/ftdetect/...
"     latex.vim
" settings {{{1
set nocompatible
set wrap lbr
let &showbreak = '▓▒░'
set cpo+=n
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
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~,*tags*
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

" appearance {{{1
set termguicolors
set t_ut= " Fix issues with background color on some terminals
set t_Co=256
set bg=dark
silent! colorscheme seoul256

" settings for plugins {{{1
let g:is_bash=1
let g:non_git_roots=["~/Dropbox/notes", "~/Dropbox/logbook"]
let g:fzf_layout = {'window': {'width': 0.9, 'height': 0.6 }}

" Used by .vim/plugin/markdown_foldlevel.vim
" 'nested' hides L_n+1 below L_n
" 'stacked' folds all headers, but treats them as same level
let g:markdown_fold_method='nested' " or 'stacked'
" keybinds {{{1
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
nnoremap <BS>   <C-^>

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
nnoremap <leader>en :Files ~/Dropbox/notes/<CR>
nnoremap <leader>es :Files ~/src/github.com/ChrisDavison/scripts<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>t :Tags<CR>
nnoremap <leader>T :BTags<CR>
nnoremap <F2> :e ~/Dropbox/notes/journal.txt<CR>:normal Go<CR>
nnoremap <F3> :e ~/Dropbox/notes/logbook.txt<CR>:normal Go<CR>

nnoremap <leader>p :call MaybeGFiles()<CR>

" Copy file basename
nnoremap <leader>cf :let @+=expand("%")<CR>
" Copy file full path
nnoremap <leader>cF :let @+=expand("%:p")<CR>
" Copy file full parent dir
nnoremap <leader>cd :let @+=expand("%:p:h")<CR>

" Easymotion configuration
nmap s <Plug>(easymotion-s)
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

" abbreviations {{{1
cnoreabbrev W w
cnoreabbrev Qa qa
cnoreabbrev E e
cnoreabbrev Q! q!
cnoreabbrev GIt Git
cnoreabbrev Set set
cnoreabbrev oedit only<bar>edit
cnoreabbrev oe only<bar>edit
cnoreabbrev BD bp<bar>bd #
cnoreabbrev BufOnly %bd\|e#

iabbrev meanstd μ±σ
iabbrev ALSO **See also**:
iabbrev <expr> DATE strftime("%Y-%m-%d")
iabbrev <expr> DATEN strftime("%Y-%m-%d %A")
iabbrev <expr> DATED strftime("%b %d")
iabbrev <expr> DATEFULL strftime("%Y-%m-%d %A")
iabbrev <expr> DATENFULL strftime("%Y %b %d")
iabbrev <expr> jhead strftime("# %Y-%m-%d")
iabbrev <expr> TIME strftime("%H:%M:%S")

" custom commands {{{1
command! CD exec "cd " . expand("%:p:h")
command! SeeAlso RG see also

" :Scratch | Open a 'scratch' buffer
command! Scratch edit ~/.scratch | normal <C-End>

" :FMT | Execute 'equalprg' on entire buffer, remembering position
command! FMT exec "normal mzgg=G`zmzzz"
nnoremap <leader>f :FMT<CR>

" :RG | grep / ripgrep {{{1
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case\ -g\ '!tags'
    command! -bang -nargs=* RG
                \ call fzf#vim#grep(
                \ 'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
                \ fzf#vim#with_preview('right:50%:hidden', '?'),
                \ <bang>0)
    command! -bang -nargs=* GREP
                \ call fzf#vim#grep(
                \ 'rg -F --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
                \ fzf#vim#with_preview('right:50%:hidden', '?'),
                \ <bang>0)
endif

" fold text {{{1
function! NeatFoldText()
    let lines_count_text = printf("‖ %4S ‖", v:foldend - v:foldstart)
    let curline = getline(v:foldstart)
    let len_text = len(curline) + len(l:lines_count_text)
    let padding = repeat(" ", ActualWindowWidth() - len_text)
    return curline . padding . lines_count_text
endfunction
set foldtext=NeatFoldText()
" autocommands {{{1
augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd BufWritePre * call MakeNonExistentDirs()
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au BufEnter * Root
    au Filetype make setlocal noexpandtab
augroup END
