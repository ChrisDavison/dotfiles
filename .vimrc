" vim: fdm=marker
let mapleader=" "

" Load plugins
execute pathogen#infect("~/.vim/bundle/{}")

" settings {{{1
set nocompatible
set wrap lbr
let &showbreak = '▓▒░'
set cpo+=n
set breakindent
set breakindentopt=shift:4,sbr
set number relativenumber
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
silent! colorscheme molokayo

" settings for plugins {{{1
let g:markdown_fenced_languages = ['python', 'rust', 'cpp', 'go']
let g:go_fmt_command="goimports"
let g:go_fmt_autosave=1
let g:go_version_warning=0
let g:pymode_python = 'python3'
let g:rustfmt_autosave=1
let g:is_bash=1
let g:tex_flavor = "latex"
let g:vimtex_compiler_progname = 'nvr'
let g:echodoc#enable_at_startup=1
let g:echodoc#type="echo"
let g:non_git_roots=["~/Dropbox/notes", "~/Dropbox/logbook"]

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
nnoremap <leader>en :Files! ~/Dropbox/notes/<CR>
nnoremap <leader>es :Files! ~/src/github.com/ChrisDavison/scripts<CR>
nnoremap <leader>b :Buffers!<CR>
nnoremap <leader>t :Tags<CR>
nnoremap <leader>T :BTags<CR>
nnoremap <F2> :e ~/Dropbox/notes/journal.txt<CR>:normal Go<CR>
nnoremap <F3> :e ~/Dropbox/notes/logbook.txt<CR>:normal Go<CR>
nnoremap <leader>l i<C-R>="[" . expand("#") . "](./" . expand("#") . ")"<CR><ESC>

nnoremap <leader>p :call <sid>maybe_gfiles()<CR>
function! s:maybe_gfiles()
    " system is only called to test for it's error code
    call system('git rev-parse --show-toplevel')
    if !v:shell_error
        GFiles!
    else 
        Files!
    endif
endfunction

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
cnoreabbrev CD cd expand("%:h")
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

" :Scratch | Open a 'scratch' buffer {{{1
command! Scratch edit ~/.scratch | normal <C-End>

" :FMT | Execute 'equalprg' on entire buffer, remembering position {{{1
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
" :Headers | Show 'function-like' things in current file {{{1
let s:headermap={
            \'rust': 'fn',
            \'python': 'def',
            \'go': 'func',
            \'vim': 'function',
            \'markdown': '#\+'}
function! s:goto_header(filter)
    let filt = len(a:filter) > 0 ? a:filter : ""
    let pattern="^\\s*" . s:headermap[&filetype] . "\\s*" .  l:filt
    exec "BLines" . pattern
endfunction
command! -nargs=* Headers exec s:goto_header(<q-args>)
nnoremap <leader>i :Headers<CR>
" saving | make non-exitent dirs (including parents) on save {{{1
function! s:makeNonExDir()
    if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h'))
        call mkdir(expand('<afile>:h'), 'p')
    endif
endfunction
" markdown | equalprg and filetype assignment {{{1
let markdown_reference_links=0
let markdown_hard_wrap=0
let md_equalprg="pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block"
let md_equalprg.=markdown_reference_links ? "-shortcut_reference_links\ --reference-links\ --reference-location=section" : ""
let md_equalprg.=markdown_hard_wrap ? "\ --columns=79\ --wrap=auto" : "\ --wrap=none"
let md_equalprg.="\ --atx-headers"

function! s:maybe_filetype_markdown()
    if &filetype == "help" || expand('%:p') =~ "doc/"
        setlocal filetype=help
    else
        setlocal filetype=markdown
    endif
endfunction

let g:markdown_fold_method='nested' " or 'stacked'
function! FoldLevelMarkdown()
    let matches_atx = matchlist(getline(v:lnum), '^\(#\+\)\s')
    let line_len = len(getline(v:lnum))
    let matches_setex_one = len(matchlist(getline(v:lnum+1), '^=\+$')) > 0
    let matches_setex_two = len(matchlist(getline(v:lnum+1), '^-\+$')) > 0
    let prev_not_blank = len(getline(v:lnum)) > 0
    if len(l:matches_atx) > 0 
        if g:markdown_fold_method == 'stacked'
            return ">1"
        else
            return ">" . len(l:matches_atx[1])
        end
    elseif l:matches_setex_one && prev_not_blank
        return ">1"
    elseif l:matches_setex_two && prev_not_blank
        if g:markdown_fold_method == 'stacked'
            return ">1"
        else
            return ">2"
        endif
    else
        return "="
    end
endfunction

function! s:markdown_backlinks()
    call fzf#vim#grep(
                \ "rg --column --line-number --no-heading --color=always --smart-case ".expand('%'), 1,
                \ fzf#vim#with_preview('right:50%:hidden', '?'), 0)
endfunction
command! Backlinks call s:markdown_backlinks()
command! SeeAlso RG see also

function! s:markdown_goto_file()
    try
        normal! vi(gf"by
        execute "edit " . getreg("b")
    catch
        echo v:exception
    catch /^Vim.*E447/
        echo "COULDN'T GOTO FILE " . v:exception
    endtry
endfunction
command! GotoFile call s:markdown_goto_file()

function! s:copy_filename_as_mdlink()
    let @a="[" . expand('%') . "](./" . expand('%') . ")"
endfunction

function! s:get_visual(only_on_line)
    let l:start_line = line("'<")
    let l:start_col = col("'<")
    let l:end_line = line("'>")
    let l:end_col = col("'>")
    if a:only_on_line && (l:start_line != l:end_line)
        echom "FileFromSelected: Start and end must be same line number"
        return
    end
    return getline(".")[l:start_col-1:l:end_col-1]
endfunction

function! s:text_around_visual()
    let start_line = line("'<")
    let start_col = col("'<")
    let end_line = line("'>")
    let end_col = col("'>")
    let before=getline(start_line)[:start_col-2]
    if start_col == 1
        let before = ""
    end
    let after=getline(start_line)[end_col:]
    return [before, after]
endfunction

function! s:make_markdown_link(text, url)
    return "[" . a:text . "](" . a:url . ")"
endfunction

function! s:sanitise_filename(filename)
    let nospace = substitute(a:filename, " ", "-", "g")
    let lower = tolower(nospace)
    let nosyms = substitute(lower, "[^a-zA-Z0-9\-]", "", "g")
    return nosyms
endfunction

function! FileFromSelected(is_visual)
    let text= a:is_visual ? s:get_visual(1) : expand('<cword>')
    let l:start_line = line(".")
    let l:start_col = col(".")
    let linktext="./" . s:sanitise_filename(l:text) . ".txt"
    let replacetext=s:make_markdown_link(l:text, linktext)
    if a:is_visual
        let around_visual = s:text_around_visual()
        let l:line=around_visual[0] . replacetext . around_visual[1]
        call setline(l:start_line, l:line)
    else
        execute "normal ciw" . l:replacetext
    end
    call cursor(l:start_line, l:start_col+1)
    return linktext
endfunction
function! EditFileFromSelected(is_visual)
    exec "w|edit " . FileFromSelected(a:is_visual)
endfunction
nnoremap ml :call FileFromSelected(0)<CR>
vnoremap ml :call FileFromSelected(1)<CR>

nnoremap gml :call EditFileFromSelected(0)<CR>
vnoremap gml :call EditFileFromSelected(1)<CR>
" fold text {{{1
function! s:actual_win_width()
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

function! NeatFoldText()
    let lines_count_text = printf("%s ι ", v:foldend - v:foldstart)
    let curline = getline(v:foldstart)
    let len_text = len(curline) + len(l:lines_count_text)
    let padding = repeat(" ", s:actual_win_width() - len_text - 2)
    return curline . " " . padding . lines_count_text
endfunction
set foldtext=NeatFoldText()
" vim templates {{{1
let g:templates_no_builtin_templates=1
let g:templates_directory=["~/.vim/templates/"]
let g:templates_user_variables=[
            \ ['CLIPBOARD', 'GetClipboard'],
            \ ['FROMFILE', 'GetAlternate'],
            \ ]
function! GetClipboard()
    return getreg("+")
endfunction
function! GetAlternate()
    return getreg("#")
endfunction
" autocommands {{{1
augroup vimrc
    autocmd!
    au TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd BufWritePre * call s:makeNonExDir()
    au CursorHold * silent! checktime " Check for external changes to files
    au VimResized * wincmd= " equally resize splits on window resize
    au BufWritePost .vimrc,init.vim source $MYVIMRC
    au BufEnter *.txt,*.md,.scratch call s:maybe_filetype_markdown()
    au BufEnter * Root
    au BufLeave *.txt,*.md call s:copy_filename_as_mdlink()
    au Filetype make setlocal noexpandtab
    au Filetype markdown setlocal foldenable foldlevelstart=0 foldmethod=expr
    au Filetype markdown setlocal foldexpr=FoldLevelMarkdown()
    au Filetype markdown setlocal conceallevel=1
    au Filetype markdown setlocal nospell
    au Filetype markdown let &l:equalprg=md_equalprg
    au Filetype markdown nnoremap gf :GotoFile<CR>
    au BufRead,BufNewFile *.latex set filetype=tex
    au Filetype tex setlocal tw=80 colorcolumn=80
    au Filetype tex setlocal equalprg=pandoc\ --from\ latex\ --to\ --latex\ --columns=80
    au FileType python setlocal foldmethod=indent
augroup END
