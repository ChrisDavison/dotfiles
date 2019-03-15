" ChrisDavison's vim config
let mapleader=" "
set runtimepath^=~/src/github.com/chrisdavison/dotfiles
set runtimepath^=~/src/github.com/chrisdavison/dotfiles/vim
" Much of this has been split into plugin and ftplugin,
" which are part of <runtimepath>.
" Most language-specific config is in there.
" =============================================================
" TODO Language server config for rust currently disabled (in ftplugin/rust)
" TODO add abbrevs for my common languages

" settings (using tpope/vim-sensible as a base)
" sensible sets...autoindent, better backspace, smarttab, timeout, incsearch,
" ruler, wildmenu, listchars, autoread, history, tabpagemax, viminfo
syntax on
filetype plugin indent on

set nocompatible
set autochdir
set wrap lbr
let &showbreak = '↳ '
set cpo+=n
set breakindent
set breakindentopt+=shift:2,sbr
set number relativenumber
set iskeyword=a-z,A-Z,_,.,39  " Used e.g. when searching for tags
set tags=./tags;,tags,.git/tags
set hidden
if has('win32')
    set shell=cmd.exe
    set shellcmdflag=/c
else
    if executable('/usr/local/bin/zsh')
        set shell=/usr/local/bin/zsh
    else
        set shell=/bin/bash
    endif
endif
if has('nvim')
    set inccommand=nosplit  " Live-preview of :s commands
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
    set undodir=~/.undodir
endif
let g:netrw_list_hide= '.*\.swp$,.DS_Store,*/tmp/*,*.so,*.swp,*.zip,*.git,^\.\.\=/\=$'
set conceallevel=2

" =====================
" general plugin config
" =====================
let g:SuperTabDefaultCompletionType = "context"
let g:fastfold_savehook = 0
let g:cd_schedule_words = [ 'TODO' , 'WAITING', 'DONE', 'CANCELLED' ]

" =======================
" Personal plugins/config
" =======================

" Plugins must be sourced (as they have built in laziness)
" and it doesn't work right with runtime
exec 'source '.globpath(&rtp, '*/junegunn*.vim')

runtime abbrev_shebang.vim
runtime abbreviations.vim
runtime appearance.vim
runtime better_digraphs.vim
runtime completion_during_search.vim
runtime custom_commands.vim
runtime cut_and_paste.vim
runtime dos2unix.vim
runtime edit_current_filetype_plugin.vim
runtime find_markdown_fold_level.vim
runtime fzf_rg_config.vim
runtime highlight_interesting_words.vim
runtime keybinds.vim
runtime load_new_plugins.vim
runtime logbook.vim
runtime make_nonexistent_dir.vim
runtime my_settings.vim
runtime new_markdown_template.vim
runtime path_jumping_commands.vim
runtime plugin
runtime read_file_template.vim
runtime scheduling.vim
runtime show_help_in_tab.vim
runtime strip_trailing_whitespace.vim
runtime test.py
runtime thesis_notes.vim
runtime toggle_color_column.vim
runtime toggle_conceal.vim

" =====================================================
" autocommands (NON file specific)
" file-specific are in DOTFILES/vim/ftplugin/<lang>.vim
" =====================================================
augroup vimrc
    autocmd!
    autocmd ColorScheme * hi! link SignColumn LineNr
    autocmd TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd CursorHold * silent! checktime " Check for external changes to files
    autocmd VimResized * wincmd= " equally resize splits on window resize
    autocmd User GoyoEnter Limelight
    autocmd User GoyoLeave Limelight!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
    autocmd BufWritePre *.md silent! call StripTrailingWhitespace()
    autocmd BufNewFile *.md exec VimNewMarkdown(expand("<afile>"))
    autocmd BufNewFile *.py call ReadFileTemplate()
augroup END

