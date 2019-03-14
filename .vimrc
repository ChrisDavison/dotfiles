" ChrisDavison's vim config
let mapleader=" "
set runtimepath^=~/src/github.com/chrisdavison/dotfiles/vim
" REMEMBER
" filetype-specific stuff is in runtimepath/ftplugin/<filetype>.vim
" This has generally been split into &rtp plugins and ftplugins
" =============================================================
" TODO Language server config for rust currently disabled (in ftplugin/rust)
" TODO add abbrevs for my common languages
" TODO plugin to yank all lines matching a grep
" TODO command/keybind to start a vimgrep like :vimgrep // *.<CURRENT FN EXTENSION>
" TODO fix 'EditPlugin' to live-filter

" Plugins must be sourced (as they have built in laziness)
" and it doesn't work right with runtime
exec 'source '.globpath(&rtp, '*/junegunn_plug_plugins.vim')

" Personal plugins/config
runtime my_settings.vim 
runtime make_nonexistent_dir.vim  
runtime logbook.vim   
runtime thesis_notes.vim
runtime scheduling.vim   
runtime toggle_color_column.vim
runtime toggle_conceal.vim
runtime new_markdown_template.vim
runtime fzf_rg_config.vim
runtime find_markdown_fold_level.vim
runtime strip_trailing_whitespace.vim 
runtime abbreviations.vim  
runtime custom_commands.vim  
runtime path_jumping_commands.vim
runtime keybinds.vim  
runtime show_help_in_tab.vim
runtime completion_during_search.vim   " Test this...
runtime abbrev_shebang.vim

" ============
" autocommands
" ============
augroup vimrc
    autocmd!
    autocmd FileType c,cpp,arduino,go,rust,javascript set foldmethod=syntax
    autocmd FileType make    set noexpandtab
    autocmd FileType vim     set foldmethod=marker
    autocmd BufWritePre *.md,*.txt,*.csv call StripTrailingWhitespace()
    autocmd BufNewFile *.md exec VimNewMarkdown(expand("<afile>"))
    autocmd BufWinEnter todo.md highlight TodoDate ctermfg=red
    autocmd BufWinEnter todo.md match TodoDate /\d\d\d\d-\d\d-\d\d/
    autocmd ColorScheme * hi! link SignColumn LineNr
    autocmd TextChanged,InsertLeave,FocusLost * silent! wall
    autocmd CursorHold * silent! checktime " Check for external changes to files
    autocmd VimResized * wincmd= " equally resize splits on window resize
    autocmd User GoyoEnter Limelight
    autocmd User GoyoLeave Limelight!
    autocmd BufWritePre * call MakeNonExDir()   " Use my rtp func
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END
" =====================
" general plugin config
" =====================
let g:SuperTabDefaultCompletionType = "context"
let g:fastfold_savehook = 0
let g:cd_schedule_words = [ 'TODO' , 'WAITING', 'DONE', 'CANCELLED' ]
