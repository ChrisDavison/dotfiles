set nocompatible
syntax on
filetype plugin indent on
let mapleader="\\"

source $HOME/.vim/vimrc/settings.vim
source $HOME/.vim/vimrc/plugins.vim
source $HOME/.vim/vimrc/appearance.vim
source $HOME/.vim/vimrc/keybinds.vim
source $HOME/.vim/vimrc/shebang.vim
source $HOME/.vim/vimrc/fzf.vim
source $HOME/.vim/vimrc/statusbar.vim
source $HOME/.vim/vimrc/syntastic.vim

" Language-specific customisation
source $HOME/.vim/vimrc/markdown.vim
source $HOME/.vim/vimrc/languages.vim
"" Lisps
source $HOME/.vim/vimrc/vimslime.vim 

" Loose functions
source $HOME/.vim/vimrc/customfuncs.vim
source $HOME/.vim/vimrc/customfold.vim

" Not yet confirmed for use
source $HOME/.vim/vimrc/totidy.vim

" Not used for now, but may return
" source $HOME/.vim/vimrc/goyo.vim


set autochdir
