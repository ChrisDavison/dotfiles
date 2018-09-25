DIR="$1"
if [ "$DIR" = "" ]; then
    echo "Must pass dotfiles directory"
    exit
fi

linkOrError(){
    target="$HOME"/"$1"
    origin="${DIR}/$1"
    if [ -f "${target}" ]; then
        echo "Replacing ${target}"
        rm "${target}"
    fi
    ln -s "${origin}" "${target}"
}
linkOrError ".zshenv"
linkOrError ".sqliterc"
linkOrError ".tmux.conf"
linkOrError ".gitconfig"
linkOrError ".vimrc"

# Clone vim plugins, using pathogen method
curl --create-dirs -sSLo ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
clone_to_bundle() {
    repo="$1"
    if [ "$OSTYPE" = "msys" ]; then
	    vimdir="vimfiles"
    else
        vimdir=".vim"
    fi
    if [ ! -d ~/"$vimdir"/bundle ]; then
        mkdir -p ~/"$vimdir"/bundle
    fi
    # echo $(echo "$repo" | sed -e "s/\//-/")
    target=~/"$vimdir"/bundle/$(echo "$repo" | sed -e "s_/_-_")
    if [ ! -d "$target" ]; then
        git clone git@github.com:"$repo" "$target" > /dev/null
    fi
}
# Individual languages {{{
clone_to_bundle fatih/vim-go
clone_to_bundle pangloss/vim-javascript
clone_to_bundle plasticboy/vim-markdown
clone_to_bundle leafgarland/typescript-vim
clone_to_bundle lervag/vimtex
clone_to_bundle mxw/vim-jsx
clone_to_bundle rust-lang/rust.vim
# }}}
# Utility {{{
clone_to_bundle christoomey/vim-tmux-navigator
clone_to_bundle airblade/vim-gitgutter
clone_to_bundle godlygeek/tabular
clone_to_bundle Konfekt/FastFold
clone_to_bundle MarcWeber/vim-addon-mw-utils
clone_to_bundle bps/vim-textobj-python
clone_to_bundle dahu/vim-fanfingtastic
clone_to_bundle dhruvasagar/vim-table-mode
clone_to_bundle easymotion/vim-easymotion
clone_to_bundle ervandew/supertab
clone_to_bundle garbas/vim-snipmate
clone_to_bundle honza/vim-snippets
clone_to_bundle jpalardy/vim-slime
clone_to_bundle junegunn/fzf.vim
clone_to_bundle kana/vim-textobj-user
clone_to_bundle kkoenig/wimproved.vim
clone_to_bundle Shougo/echodoc.vim
clone_to_bundle paulhybryant/vim-textobj-path
clone_to_bundle terryma/vim-expand-region
clone_to_bundle tomtom/tlib_vim
clone_to_bundle tpope/vim-commentary
clone_to_bundle tpope/vim-dispatch
clone_to_bundle tpope/vim-fugitive
clone_to_bundle tpope/vim-obsession
clone_to_bundle tpope/vim-sensible
clone_to_bundle tpope/vim-surround
clone_to_bundle tpope/vim-unimpaired
clone_to_bundle tpope/vim-vinegar
clone_to_bundle wellle/targets.vim
clone_to_bundle w0rp/ale
clone_to_bundle mattn/webapi-vim
clone_to_bundle prabirshrestha/async.vim
clone_to_bundle prabirshrestha/vim-lsp
clone_to_bundle prabirshrestha/asyncomplete.vim
clone_to_bundle prabirshrestha/asyncomplete-lsp.vim
# }}} 
# Themes {{{
clone_to_bundle dracula/vim
clone_to_bundle nielsmadan/harlequin
clone_to_bundle nanotech/jellybeans.vim
clone_to_bundle owickstrom/vim-colors-paramount
clone_to_bundle junegunn/seoul256.vim
clone_to_bundle gosukiwi/vim-atom-dark
clone_to_bundle morhetz/gruvbox
clone_to_bundle sickill/vim-monokai
clone_to_bundle colepeters/spacemacs-theme.vim
# }}}

# Install FZF
if [ ! -d "$HOME/.vim/bundle/fzf" ]; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.vim/bundle/fzf
else
    echo "FZF already cloned"
    cd ~/.vim/bundle/fzf
    git pull --rebase > /dev/null
fi
~/.vim/bundle/fzf/install --all > /dev/null
