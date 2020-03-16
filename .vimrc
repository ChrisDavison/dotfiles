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
set foldlevelstart=99
set autochdir

set cmdheight=2
set shortmess+=c


" Some servers have issues with backup files
set nobackup nowritebackup

set directory=~/.temp,.
set ignorecase smartcase " ignore case unless i specifically mix letter case
set wildmode=list:longest:full,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~,*tags*
set wildignore+=*.swp,*.so,*.fls,*.log,*.out,*.toc,*.xdv,*.bbl,*.blg,*.fdb_latexmk
set wildignorecase
set nojoinspaces   " don't autoinsert two spaces after '.' etc in join
set switchbuf=useopen,usetab
set splitbelow splitright
set showmode
let g:netrw_list_hide=netrw_gitignore#Hide() . '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$,.mypy_cache,__pycache__,.*\.aux,.*\.log,.*\.bbl,.*\.blg,.*\.fdb_latexmk,.*\.fls,.*\.log,.*\.out,.*\.toc'

set signcolumn=yes
set path=.,**
set statusline=\ %t:%l:%c\ %m%r\ %{FugitiveStatusline()}
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
if !has('gui_running')
    set t_Co=256
endif
set bg=light
silent! colorscheme yang
" settings for plugins {{{1
let g:is_bash=1
let g:fzf_layout = {'window': 'enew'}

" Used by .vim/plugin/markdown_foldlevel.vim
" 'nested' -- hides L_n+1 below L_n
" 'stacked' -- folds all headers, but treats them as same level
let g:markdown_fold_method='nested'

" From .vim/plugin/foldtext
set foldtext=CustomFoldText()

if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case\ -g\ '!tags'
endif

let &rtp.=",~/.vim/snippets"
let g:UltiSnipsSnippetDirectories=["~/.vim/UltiSnips"]

let g:gutentags_project_root = ['tags']
let g:gutentags_define_advanced_commands=1


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
" nmap s <Plug>(easymotion-sn)

" Run 'equalprg' and return to mark
nnoremap <leader>F :normal mzgg=G`zmzzz<CR>

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
nnoremap <UP> :resize +2<CR>
nnoremap <DOWN> :resize -2<CR>
nnoremap <LEFT> :vertical resize +2<CR>
nnoremap <RIGHT> :vertical resize -2<CR>
"      terminal {{{1
tnoremap <Esc> <C-\><C-n>
"      fzf {{{1
let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-x': 'split',
            \ 'ctrl-v': 'vsplit' }
"      files, SPECIFIC files/dirs, buffers, tags {{{1
nnoremap <leader>en :Files ~/Dropbox/notes/<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>t :Tags<CR>
nnoremap <leader>T :BTags<CR>
nnoremap <leader>k :Tagsearch<CR>
nnoremap <leader>K :exec "Rg " . expand('<cWORD>')<CR>
nnoremap <leader>p :call MaybeGFiles()<CR>
nnoremap <leader>r :Rg 
" ctags definitions for markdown urls and @keywords
nnoremap <leader>l :BTags link <CR>
nnoremap <leader># :Tags @<CR>

"      for my plugins (~/.vim/plugin) {{{1
let g:fzf_favourite_files = [
            \ {"name": "index of notes", "path": "~/Dropbox/notes/index.txt"},
            \ {"name": "journal", "path": "~/Dropbox/notes/journal.txt"},
            \ {"name": "logbook", "path": "~/Dropbox/notes/logbook.txt"},
            \ {"name": "stuff to learn", "path": "~/Dropbox/notes/stuff-to-learn.txt"},
            \ {"name": "calendar", "path": "~/Dropbox/notes/calendar.txt"},
            \ {"name": "todos", "path": "~/Dropbox/notes/todo.txt"},
            \ {"name": "todos → projects", "path": "~/Dropbox/notes/projects.txt"},
            \ {"name": "todos → thesis", "path": "~/Dropbox/notes/thesis.txt"},
            \ {"name": "habits STACKS", "path": "~/Dropbox/notes/my-habit-stacks.txt"},
            \ {"name": "habits", "path": "~/Dropbox/notes/my-habits.txt"},
            \ {"name": "vimrc", "path": "~/.vimrc"},
            \]
nnoremap <leader>f :Favourites<CR>
nnoremap <F2> :e ~/Dropbox/notes/todo.txt<CR>
nnoremap <F3> :silent!only<BAR>silent!edit ~/Dropbox/notes/index.txt<CR>
nnoremap <leader>il :InsertLinkToNote 

"      copy file basename, full-path, or parent dir {{{1
nnoremap <leader>cf :let @+=resolve(expand("%"))<CR>
nnoremap <leader>cF :let @+=resolve(expand("%:p"))<CR>
nnoremap <leader>cd :let @+=resolve(expand("%:p:h"))<CR>
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
iabbrev <expr> DATE strftime("%Y-%m-%d")
iabbrev <expr> DATEN strftime("%Y-%m-%d %a")
iabbrev <expr> TIME strftime("%H:%M:%S")
iabbrev <expr> jhead strftime("# %Y-%m-%d %A")
" custom commands {{{1
command! CD exec "cd " . expand("%:p:h")
command! SeeAlso Rg see also
command! Scratch edit ~/Dropbox/notes/.scratch | normal <C-End>
command! BD bp|bd#
cnoreabbrev Bd BD
" autocommands {{{1
augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd BufWritePre * call file#make_nonexistent_dirs()
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au Filetype make setlocal noexpandtab
augroup END
" }}}1
" coc.nvim {{{1
let g:suggest#enablePreview='true'
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "<TAB>" :
            \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()


" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
"
" position. Coc only does snippet and additional edit on confirm.
if has('patch8.1.1068')
  " Use `complete_info` if your (Neo)Vim version supports it.
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

command! -bang ThirdPerson call setqflist([], 'r', {'lines': systemlist('thirdperson.sh ' . (<bang>0 ? '*.tex' : expand('%')))})<BAR>:copen
command! -bang Passive call setqflist([], 'r', {'lines': systemlist('passive.sh ' . (<bang>0 ? '*.tex' : expand('%')))})<BAR>:copen
command! -bang Weasel call setqflist([], 'r', {'lines': systemlist('weasel.sh ' . (<bang>0 ? '*.tex' : expand('%')))})<BAR>:copen

command! EFiletype exec "edit ~/.vim/after/ftplugin/" . &filetype . ".vim"
