filetype plugin indent on
syntax enable
let mapleader=" "

execute pathogen#infect("~/.vim/bundle/{}")
" settings {{{1
set nocompatible
let &showbreak = '····'
set cpo+=n
set number
set wrap lbr
set autoindent
set breakindent
set breakindentopt=shift:4,sbr
set backspace=indent,eol,start
set iskeyword=a-z,A-Z,_  " Used e.g. when searching for tags
setglobal tags-=./tags tags-=./tags; tags^=./tags;
set incsearch
set updatetime=300 " Write a swap file after 1 second
set autoread
set tabstop=4 softtabstop=4 shiftround shiftwidth=4 expandtab
set clipboard+=unnamedplus " Use system clipboard with vim clipboard
set lazyredraw " Don't redraw while executing macros
set foldlevelstart=99
set autochdir

set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set cmdheight=2
set shortmess+=c

set scrolloff=1
set sidescrolloff=5
set display+=lastline

" Some servers have issues with backup files
set nobackup nowritebackup
set history=1000
set tabpagemax=50
if !empty(&viminfo)
    set viminfo^=!
endif
set sessionoptions-=options
set viewoptions-=options

set directory=~/.temp,.
set ignorecase smartcase " ignore case unless i specifically mix letter case
set wildmenu
set wildmode=longest:list,full
set wildignore+=*DS_Store*,*.png,*.jpg,*.gif,*.aux,*.*~,*tags*
set wildignore+=*.swp,*.so,*.fls,*.log,*.out,*.toc,*.xdv,*.bbl,*.blg,*.fdb_latexmk
set wildignorecase
set nojoinspaces   " don't autoinsert two spaces after '.' etc in join
set switchbuf=useopen,usetab
set splitbelow splitright
set showmode
let g:netrw_list_hide=netrw_gitignore#Hide() . '.*\.swp$,\.DS_Store,*.so,*.zip,\.git,\~$,.mypy_cache,__pycache__,.*\.aux,.*\.log,.*\.bbl,.*\.blg,.*\.fdb_latexmk,.*\.fls,.*\.log,.*\.out,.*\.toc'

set smarttab
set nrformats-=octal
set formatoptions+=j
set formatoptions-=a
set signcolumn=yes
set path=.,**
set laststatus=2
set statusline=\ %t:%l:%c\ %m%r\ %{FugitiveStatusline()}
set ruler
set encoding=utf-8

if !exists('g:loaded_matchit')
    runtime! macros/matchit.vim
endif
"      undo (save undo history across sessions) {{{1
set undodir=~/.undodir
set undofile
set complete-=i
set completeopt=menu,menuone,preview

if !has('nvim') && &ttimeoutlen == -1
  set ttimeout
  set ttimeoutlen=100
endif

"      shell (specialised per os) {{{1
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
else
    let shells=['/usr/bin/zsh', '/usr/bin/fish', '/usr/bin/bash', '/bin/bash']
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
set t_Co=16
if !has('gui_running')
    set t_Co=256
endif
let g:lightline={'colorscheme':"seoul256"}
let g:molokai_original=1
let g:rehash256 = 1
set bg=dark
silent! colorscheme seoul256
"      plugins {{{1
let g:is_bash=1
let g:fzf_layout = {'down': '~40%'}
let g:fzf_preview_window=''
let g:checkmark_no_mappings=1
if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case\ -g\ '!tags'
endif

let g:gutentags_project_root = ['tags']
let g:gutentags_define_advanced_commands=1
"      markdown {{{1

" 'nested' -- hides L_n+1 below L_n
" 'stacked' -- folds all headers, but treats them as same level
let g:markdown_fold_method='nested'

let g:markdown_fenced_languages = ['python', 'rust', 'cpp', 'go']
let g:pandoc#formatting#mode='hA'
let g:pandoc#keyboard#use_default_mappings=0
let g:pandoc#formatting#smart_autoformat_on_cursormoved=1

let g:markdown_reference_links=0
let g:markdown_hard_wrap=1

let md_equalprg="pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block"
let md_equalprg.=g:markdown_reference_links ? "-shortcut_reference_links\ --reference-links\ --reference-location=section" : ""
let md_equalprg.=g:markdown_hard_wrap ? "\ --columns=79\ --wrap=auto" : "\ --wrap=none"
let md_equalprg.="\ --atx-headers"

" let &l:equalprg=md_equalprg
let g:pandoc#formatting#equalprg=md_equalprg
let g:pandoc#formatting#extra_equalprg=''
let g:pandoc#modules#disabled = ['hypertext', 'spell']
let g:pandoc#folding#fdc=0
let g:pandoc#folding#fold_fenced_codeblocks=1
let g:pandoc#syntax#conceal#urls=0
let g:pandoc#syntax#conceal#blacklist=['ellipses', 'atx', 'subscript', 'superscript', 'strikeout', 'codeblock_start', 'codeblock_delim', 'footnote', 'definition', 'list']
let g:pandoc#spell#enabled=0

augroup markdown
    au!
    au BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
    au Filetype markdown,markdown.pandoc setlocal foldenable 
                \ foldmethod=expr foldlevelstart=1 
                \ nospell conceallevel=1
                \ formatoptions+=a textwidth=79
    au Filetype markdown,markdown.pandoc nnoremap <buffer> gf :call Markdown_goto_file(0)<CR>
    au Filetype markdown,markdown.pandoc nnoremap <buffer> gs :call Markdown_goto_file(2)<CR>
    au Filetype markdown,markdown.pandoc nnoremap <buffer> <leader>gf :call Markdown_goto_file(0)<CR>
    au Filetype markdown,markdown.pandoc nnoremap <buffer> <leader>gs :call Markdown_goto_file(1)<CR>
    au Filetype markdown,markdown.pandoc CocDisable
    au Filetype markdown,markdown.pandoc command! -bang Backlinks call Markdown_backlinks(<bang>1)
    au Filetype markdown,markdown.pandoc nnoremap <buffer> <leader>B :Backlinks!<CR>
    au Filetype markdown,markdown.pandoc let &l:equalprg=md_equalprg
augroup end

function! Markdown_goto_file(split) " {{{2
    let fname=expand("<cfile>")
    let command = "edit "
    if a:split > 0
        if ActualWindowWidth() > 160
            let command = "vsplit "
        else
            let command = "split "
        endif
    endif
    if filereadable(l:fname)
        execute "silent!" . l:command . l:fname
    else
        if getline(".")[col(".")] != "]"
            normal f]
        end
        normal vi("by
        if filereadable(getreg("b"))
            execute "silent!" . l:command . getreg("b")
        else
            echom "Couldn't find valid link."
        end
    end
endfunction " 

function! Markdown_backlinks(use_grep) " {{{2
    if a:use_grep
        exec "silent grep! '\\((\./)*" . expand("%") . "'"
    else
        call fzf_vim_grep(
        \ "rg --column --line-number --no-heading --color=always --smart-case -g '!tags' ".expand('%'), 1,
        \ fzf_vim_with_preview('right:50%:hidden', '?'), 0)
    end
endfunction " 

function! Markdown_copy_filename_as_link() " {{{2
    let link=s:make_markdown_link(expand('%'), "./" . expand('%'))
    let @a=l:link
endfunction " 

"      golang {{{1
let g:go_fmt_command="goimports"
let g:go_fmt_autosave=1
let g:go_version_warning=0
augroup go
    au!
    au Filetype go set foldmethod=syntax
augroup end
"      python {{{1
let g:pymode_python = 'python3'
augroup python
    au! Filetype python set foldmethod=indent
    au! Filetype python set formatoptions-=a
augroup end
"      rust {{{1
let g:rustfmt_autosave=1
augroup rust
    au!
    au Filetype rust set foldmethod=syntax
augroup end
"      latex {{{1
let g:vimtex_format_enabled=1
let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'

augroup latex
    au!
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex set foldmethod=expr
                \ foldexpr=vimtex#fold#level(v:lnum)
                \ foldtext=vimtex#fold#text()
                \ fillchars=fold:\  
                \ formatoptions-=a
    au Filetype tex command! Todos :silent grep \\\\todo<CR>
    au Filetype tex command! BTodos :silent lgrep \\\\todo %<CR>
    au Filetype tex command! Tables :silent grep '\\\\begin\{table'<CR>
    au Filetype tex command! BTables :silent lgrep '\\\\begin\{table' %<CR>
    au Filetype tex command! Figures :silent grep '\\\\begin\{figure'<CR>
    au Filetype tex command! BFigures :silent lgrep '\\\\begin\{figure' %<CR>
augroup end
"      Todo.txt  {{{1 
augroup todotxt
    au!
    au BufNewFile,BufFilePre,BufRead todo.txt,done.txt set filetype=todo.txt 
    au Filetype todo.txt,text setlocal formatoptions -=a
augroup end
" keybinds {{{1 
nnoremap <silent> Q =ip
nnoremap S      :%s///<LEFT>
vnoremap S      :s///<LEFT>
vnoremap <      <gv
vnoremap >      >gv
nnoremap <expr> j      (v:count == 0? 'gj' : 'j')
nnoremap <expr> k      (v:count == 0? 'gk' : 'k')
nnoremap D      dd
nnoremap Y      y$
nnoremap <BS>   <C-^>
nnoremap <TAB>  za
vnoremap W      :w <BAR>norm gvD<LEFT><LEFT><LEFT><LEFT><LEFT><LEFT><LEFT><LEFT><LEFT>

if empty(mapcheck('<C-U>', 'i'))
    inoremap <C-U> <C-G>u<C-U>
endif

if empty(mapcheck('<C-W>', 'i'))
    inoremap <C-W> <C-G>u<C-W>
endif
" nmap s <Plug>(easymotion-sn)

" Run 'equalprg' and return to mark
nnoremap <leader>F :normal mzgg=G`zmzzz<CR>

" <C-C> doesn't trigger InsertLeave autocmd, so rebind to esc
inoremap <C-c> <ESC>

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
nnoremap <leader>ev :e ~/.vimrc<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>T :Tags<CR>
nnoremap <leader>t :BTags<CR>
nnoremap <leader>k :Tagsearch<CR>
nnoremap <leader>K :exec "Rg " . expand('<cWORD>')<CR>
nnoremap <leader>p :Files<CR>

" ctags definitions for markdown urls and @keywords
nnoremap <leader>l :BTags link <CR>
nnoremap <leader># :Tags @<CR>
" abbreviations {{{1
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() =~# '^grep')  ? 'silent grep'  : 'grep'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() =~# '^lgrep') ? 'silent lgrep' : 'lgrep'
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
" Navigate 'Favourite' files {{{1
let g:favourite_files = [
        \ {"name": "INDEX", "path": "~/Dropbox/notes/index.md"},
        \ {"name": "TODO", "path": "~/Dropbox/notes/todo.txt"},
        \ {"name": "INBOX", "path": "~/Dropbox/notes/inbox.md"},
        \ {"name": "logbook", "path": "~/Dropbox/notes/logbook.md"},
        \ {"name": "wishlist", "path": "~/Dropbox/notes/want.txt"},
        \ {"name": "stuff to learn", "path": "~/Dropbox/notes/stuff-to-learn.md"},
        \ {"name": "calendar", "path": "~/Dropbox/notes/calendar.md"},
        \ {"name": "projects", "path": "~/Dropbox/notes/projects.md"},
        \]
" nnoremap <leader>f :Favourites<CR>
cnoreabbrev F Fav
nnoremap <leader>f :Fav 

function! s:edit_favourite(key)
    let names=map(copy(g:favourite_files), {_, v -> v["name"]})
    let paths=map(copy(g:favourite_files), {_, v -> v["path"]})
    let idx=index(names, a:key)
    if idx == -1
        return
    end
    let filename = expand(paths[idx])
    if isdirectory(l:filename)
        exec "Explore " . l:filename
    elseif filereadable(l:filename)
        exec "e " . l:filename
    else
        echom "Can't open file/dir " . l:filename
    endif
endfunction

command! Favourites call fzf#run(fzf#wrap({
            \ 'source': reverse(map(copy(g:favourite_files), {_, v -> v["name"]})), 
            \ 'sink': function("<SID>edit_favourite"),
            \ 'options': '--prompt Favourite: '}))

" Simpler version, not using FZF (faster, but less hand-holdy)
function! s:favourite_files(A, L, P)
    let paths=map(copy(g:favourite_files), {_, v -> v["name"]})
    let paths_filtered=filter(l:paths, {_, val -> val =~ "^" . a:A})
    return paths_filtered
endfunction

command! -nargs=1 -complete=customlist,<sid>favourite_files Fav call <sid>edit_matching(g:favourite_files, <q-args>)
cnoreabbrev fav Fav

" Navigate common 'configuration' files {{{1
let g:my_configs=[
            \ {"name": "vim", "path": "$HOME/.vimrc"},
            \ {"name": "bspwm", "path": "$HOME/.config/bspwm/bspwmrc"},
            \ {"name": "sxkhd", "path": "$HOME/.config/sxhkd/sxhkdrc"},
            \ {"name": "polybar", "path": "$HOME/.config/polybar/config"},
            \ {"name": "todo", "path": "$HOME/.todo/config"},
            \ {"name": "dotfiles", "path": "$HOME/code/dotfiles"},
            \ {"name": "fish", "path": "$HOME/.config/fish/config.fish"},
            \ {"name": "zsh", "path": "$HOME/.zshrc"},
            \ {"name": "alacritty", "path": "$HOME/.config/alacritty/alacritty.yml"},
            \ {"name": "polybar", "path": "$HOME/.config/polybar/config"},
\]


function! s:config_files(A, L, P)
    let paths=map(copy(g:my_configs), {_, v -> v["name"]})
    let paths_filtered=filter(l:paths, {_, val -> val =~ "^" . a:A})
    return paths_filtered
endfunction

function! s:edit_matching(dict, name)
    let matching=filter(copy(a:dict), {_, v -> v["name"] == a:name})[0]
    let matchingpath=expand(l:matching["path"])
    if filereadable(l:matchingpath)
        exec "edit " . l:matchingpath
    else
        echom "File not readable: " . l:matchingpath
    endif
endfunction

command! -nargs=1 -complete=customlist,<sid>config_files Conf call <sid>edit_matching(g:my_configs, <q-args>)
cnoreabbrev conf Conf
nnoremap <leader>C :Conf 


" Navigate links in markdown buffers {{{1
function! s:open_link_from_buffer(line)
    let matches=matchlist(a:line, "](\\(.*\\))")
    if len(matches) > 1
        let match = matches[1]
        if filereadable(l:match)
            exec "e " . l:match
        else
            exec "!firefox " . l:match
        end
    endif
endfunction

command! -bang LinksInBuffer call fzf#run(fzf#wrap({
            \ 'source': 'rg --only-matching "\[.*\]\(.*\)" ' . expand("%"),
            \ 'sink': function("<SID>open_link_from_buffer"),
            \ 'options': '--exact --prompt Link: '}))
" Navigate '@tags' using `Tagsearch` {{{1
function! s:find_tag(tag)
    exec "silent!Rg @" . a:tag
    call feedkeys("i")
endfunction

function! s:find_tag_grep(tag)
    exec "silent grep @" . a:tag
endfunction

command! -bang Tagsearch call fzf#run(fzf#wrap({
            \ 'source': 'tagsearch --long', 
            \ 'sink': <bang>0 ? function("<SID>find_tag") : function("<SID>find_tag_grep"),
            \ 'options': '--prompt Tag: '}))
" Insert a link to a note, with first line as title {{{1
function! s:FirstLineFromFileAsLink(filename)
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
    let lfo=&formatoptions
    set fo-=a
    exec "normal a" . l:link
    let &fo=lfo
endfunction

command! -complete=file -nargs=1 InsertLinkToNote call <SID>FirstLineFromFileAsLink(<q-args>)
nnoremap <leader>il :InsertLinkToNote 


" window width {{{1
function! ActualWindowWidth()
    return winwidth(0) - s:NumberColumnWidth() - &foldcolumn - s:SignsWidth()
endfunction

function! s:SignsWidth()
    let l:signs_width = 0
    if has('signs')
        " This seems to be the only way to find out if the signs column is even
        " showing.
        let l:signs = []
        let l:signs_string = ''
        redir =>l:signs_string|exe "sil sign place buffer=".bufnr('')|redir end
        let l:signs = split(l:signs_string, "\n")[1:]

        if !empty(signs)
            let l:signs_width = 2
        endif
    endif

    return l:signs_width
endfunction

function! s:NumberColumnWidth()
    let l:number_col_width = 0
    if &number
        let l:number_col_width = max([strlen(line('$')) + 1, 3])
    elseif &relativenumber
        let l:number_col_width = 3
    endif

    if l:number_col_width != 0
        let l:number_col_width = max([l:number_col_width, &numberwidth])
    endif

    return l:number_col_width
endfunction
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

" QuickFix utils - "Grammar Police" and nonexistent links {{{1
command! -bang BadLinks call setqflist([], 'r', {'lines': systemlist('nonexistent_notes.py --vimgrep ' . (<bang>1 ? '*.md' : expand('%')))})<BAR>:copen
command! -bang ThirdPerson call setqflist([], 'r', {'lines': systemlist('thirdperson.sh ' . (<bang>0 ? '*.tex' : expand('%')))})<BAR>:copen
command! -bang Passive call setqflist([], 'r', {'lines': systemlist('passive.sh ' . (<bang>0 ? '*.tex' : expand('%')))})<BAR>:copen
command! -bang Weasel call setqflist([], 'r', {'lines': systemlist('weasel.sh ' . (<bang>0 ? '*.tex' : expand('%')))})<BAR>:copen
" autocommands {{{1
augroup vimrc
    autocmd!
    au InsertEnter * set norelativenumber
    au InsertLeave * set relativenumber
    au TextChanged,InsertLeave,FocusLost * silent! wall
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au Filetype make set noexpandtab
    au Filetype text set formatoptions-=a
    au Filetype vim set foldmethod=marker
    au ColorScheme * call lightline#colorscheme()
    au Filetype zsh,bash,sh set foldmethod=marker
augroup END
" NEW {{{1 

