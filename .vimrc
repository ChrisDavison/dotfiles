let mapleader=" "

" .vim/autoload...
"     markdown(fold_level, backlinks, gotofile, file_from_selection)
"     selection(visual, before_and_after_visual)
"     file(make_nonexistent_dir)
"     sanitise(filename)
" .vim/after/ftplugin...
"     markdown,  tex
" .vim/plugin
"     foldtext, maybe_gfiles, window_width, fzf_favourite_files
" .vim/ftdetect...
"     markdown, latex

" Load plugins from submodules (using tpope/pathogen.vim)
execute pathogen#infect("~/.vim/bundle/{}")
" settings {{{1
set nocompatible
let &showbreak = '····'
set cpo+=n
set number
set wrap lbr
set breakindent
set breakindentopt=shift:4,sbr
set iskeyword=a-z,A-Z,_  " Used e.g. when searching for tags
set updatetime=300 " Write a swap file after 1 second
set tabstop=4 softtabstop=4 shiftround shiftwidth=4 expandtab
set clipboard+=unnamedplus " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw while executing macros

" Some servers have issues with backup files
set nobackup nowritebackup

set directory=~/.temp,.
set ignorecase smartcase " ignore case unless i specifically mix letter case
set wildmode=list:longest:list,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~,*tags*
set wildignore+=*.swp,*.so,*.fls,*.log,*.out,*.toc,*.xdv,*.bbl,*.blg,*.fdb_latexmk
set wildignorecase
set nojoinspaces   " don't autoinsert two spaces after '.' etc in join
set switchbuf=useopen,usetab
set splitbelow splitright
set showmode
let g:netrw_list_hide=netrw_gitignore#Hide() . '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$,.mypy_cache,__pycache__,.*\.aux,.*\.log,.*\.bbl,.*\.blg,.*\.fdb_latexmk,.*\.fls,.*\.log,.*\.out,.*\.toc'

set signcolumn=auto
set path=.,**
set statusline=\ %F:%l:%c
"      undo (save undo history across sessions) {{{1
set undodir=~/.undodir
set undofile
set completeopt=menu,menuone,preview
"      shell (specialised per os) {{{1
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
"      appearance {{{1
set termguicolors
set t_ut= " Fix issues with background color on some terminals
set bg=dark
if !has('gui_running')
    set t_Co=256
endif
silent! colorscheme yin
" settings for plugins {{{1
let g:is_bash=1
let g:fzf_layout = {'window': '10new'}

" Used by .vim/plugin/markdown_foldlevel.vim
" 'nested' hides L_n+1 below L_n
" 'stacked' folds all headers, but treats them as same level
let g:markdown_fold_method='nested' " or 'stacked'

let g:EasyMotion_smartcase=1

" From .vim/plugin/foldtext
set foldtext=CustomFoldText()

if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case\ -g\ '!tags'
endif

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
nnoremap <BS>   <C-^>
nnoremap <TAB>  za
nmap s <Plug>(easymotion-sn)

" Run 'equalprg' and return to mark
nnoremap <leader>f :normal mzgg=G`zmzzz<CR>

" <C-C> doesn't trigger InsertLeave autocmd, so rebind to esc
inoremap <C-c> <ESC>

" Automatically use first spelling suggestion
nnoremap <leader>s  z=1<CR><CR>

" Close quickfix or location window
nnoremap <leader>cc :cclose<bar>lclose<CR>

"      window split navigation {{{1
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
"      terminal {{{1
tnoremap <Esc> <C-\><C-n>
"      fzf {{{1
imap <C-x><C-k> <plug>(fzf-complete-word)
imap <C-x><C-f> <plug>(fzf-complete-path)
imap <C-x><C-j> <plug>(fzf-complete-file)
let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-x': 'split',
            \ 'ctrl-v': 'vsplit' }
"      files, SPECIFIC files/dirs, buffers, tags {{{1
nnoremap <leader>en :Files ~/Dropbox/notes/<CR>
nnoremap <leader>es :Files ~/src/github.com/ChrisDavison/scripts<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>t :Tags<CR>
nnoremap <leader>T :BTags<CR>
nnoremap <leader>k :Tagsearch<CR>
nnoremap <leader>K :exec "Rg " . expand('<cWORD>')<CR>
nnoremap <leader>p :call MaybeGFiles()<CR>
nnoremap <leader>r :Rg 
nnoremap <leader>r :Rg 

let g:fzf_favourite_files = [
            \ {"name": "index of notes", "path": "~/Dropbox/notes/index.txt"},
            \ {"name": "journal", "path": "~/Dropbox/notes/journal.txt"},
            \ {"name": "logbook", "path": "~/Dropbox/notes/logbook.txt"},
            \ {"name": "stuff to learn", "path": "~/Dropbox/notes/stuff-to-learn.txt"},
            \ {"name": "calendar", "path": "~/Dropbox/notes/calendar.txt"},
            \ {"name": "todos", "path": "~/Dropbox/notes/todo.txt"},
            \ {"name": "todos → projects", "path": "~/Dropbox/notes/projects.txt"},
            \ {"name": "todos → thesis", "path": "~/Dropbox/notes/thesis.txt"},
            \ {"name": "habits → STACKS", "path": "~/Dropbox/notes/my-habit-stacks.txt"},
            \ {"name": "habits → daily", "path": "~/Dropbox/notes/daily.txt"},
            \ {"name": "habits → weekly", "path": "~/Dropbox/notes/weekly.txt"},
            \ {"name": "habits → monthly", "path": "~/Dropbox/notes/monthly.txt"},
            \ {"name": "vimrc", "path": "~/.vimrc"},
            \]
nnoremap <leader>F :Favourites<CR>
nnoremap <F2> :e ~/Dropbox/notes/todo.txt<CR>
nnoremap <F3> :silent!only<BAR>silent!edit ~/Dropbox/notes/index.txt<CR>

"      copy file basename, full-path, or parent dir {{{1
nnoremap <leader>cf :let @+=resolve(expand("%"))<CR>
nnoremap <leader>cF :let @+=resolve(expand("%:p"))<CR>
nnoremap <leader>cd :let @+=resolve(expand("%:p:h"))<CR>
" abbreviations - command mode {{{1
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
cnoreabbrev ts Tagsearch
cnoreabbrev fav Favourites
" abbreviations - insert mode {{{1
iabbrev meanstd μ±σ
iabbrev ALSO **See also**:
iabbrev <expr> DATE strftime("%Y-%m-%d")
iabbrev <expr> DATEN strftime("%Y-%m-%d %A")
iabbrev <expr> DATED strftime("%b %d")
iabbrev <expr> DATEFULL strftime("%Y-%m-%d %A")
iabbrev <expr> DATENFULL strftime("%Y %b %d")
iabbrev <expr> jhead strftime("# %Y-%m-%d")
iabbrev <expr> TIME strftime("%H:%M:%S")
iabbrev TODO **TODO** -
" custom commands {{{1
command! CD exec "cd " . expand("%:p:h")
command! SeeAlso Rg see also
command! Scratch edit ~/Dropbox/notes/.scratch | normal <C-End>
" autocommands {{{1
augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd BufWritePre * call file#make_nonexistent_dirs()
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au BufEnter * Root
    au Filetype make setlocal noexpandtab
    au BufEnter logbook.txt,journal.txt setlocal foldlevelstart=0
augroup END

function! FirstLineFromFileAsLink(filename)
    let title=trim(system('head -n1 ' . a:filename))
    let matches = matchlist(title, '#\+ \(.*\)')
    if len(l:matches) > 1
        let l:title = l:matches[1]
    endif
    let filename=resolve(expand(a:filename))
    if l:filename[0] != '.'
        let filename = './' . a:filename
    endif
    let link="[" . title . "](" . a:filename . ")"
    exec "normal a" . l:link
endfunction

command! -complete=file -nargs=1 InsertLinkToNote call FirstLineFromFileAsLink(<q-args>)
nnoremap <leader>il :InsertLinkToNote 

command! MODIFY set noro modifiable
command! NOMODIFY set ro nomodifiable

let &rtp.=",~/.vim/snippets"
let g:UltiSnipsSnippetDirectories=["~/.vim/UltiSnips"]

nnoremap <leader>l :BTags ^l:<CR>
