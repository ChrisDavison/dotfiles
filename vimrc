" Author: Chris Davison
" Comments: My Vim config.  Not much language-specific stuff in this, as the
" plugin defaults seem to be pretty good in most cases.  For the record, using
" JuneGunn's 'plugged' plugin manager, as I find it's simplistic approach the
" most appealling.  If your fold is set to "marker" for the "vim" filetype,
" then this should fold in a relatively well explained manner, allowing you to
" find and change whatever you need.

" Set leader as space.  It's big, it's in the middle, and it's easy from the
" home row
let mapleader=","

" Sourced ----- {{{1
source ~/.dotfiles/myvim/init.vim
source ~/.dotfiles/myvim/plugins.vim
source ~/.dotfiles/myvim/appearance.vim
source ~/.dotfiles/myvim/bindings.vim
source ~/.dotfiles/myvim/code_folding.vim
source ~/.dotfiles/myvim/languages.vim
source ~/.dotfiles/myvim/filetype_management.vim
source ~/.dotfiles/myvim/unite.vim
source ~/.dotfiles/myvim/tagbar_markdown.vim
source ~/.dotfiles/myvim/tablemode.vim
