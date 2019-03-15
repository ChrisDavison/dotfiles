" indent/de-dent visual selection
vnoremap < <gv
vnoremap > >gv
" Mostly stuff from FZF for navigating buffers
nnoremap <leader>b :Buffers<Cr>
nnoremap <leader>p :Files<Cr>
nnoremap <leader>g :GFiles<Cr>
nnoremap <leader>ll :Lines<cr>
nnoremap <leader>lb :BLines<cr>
nnoremap <leader>m :Marks<cr>
nnoremap <leader>ta :Tags<CR>
nnoremap <leader>tb :BTags<CR>
nnoremap <leader>= gqap
nnoremap <leader>n :NOH<CR>
" easily search/replace using last search
nmap S :%s///<LEFT>
vnoremap S :s///<LEFT>
" Other bindings
nnoremap <leader>sv :source ~/src/github.com/chrisdavison/dotfiles/.vimrc<CR>
nnoremap <leader>ev :edit ~/src/github.com/chrisdavison/dotfiles/.vimrc<CR>
nnoremap <leader>en :Files ~/Dropbox/notes<CR>
nnoremap <leader>ep :Files ~/src/github.com/chrisdavison/dotfiles/vim/<CR>

nnoremap <leader>ss :mksession! ~/Dropbox/session.vim<BAR>echo "Saved session to dropbox"<CR>
nnoremap <Leader>hh :set list!<BAR>echo "Toggle hidden characters"<CR>
nnoremap <Leader>w :set wrap!<BAR>echo "Toggling line wrapping"<CR>
nnoremap <BS> <C-^>

" Uppercase the current word (from anywhere within the <word>)
inoremap <C-u> <esc>mzgUiw`za

" Keymap for the better_digraphs plugin (in rtp)
inoremap <expr> <C-K> BDG_GetDigraph()

" Panic button (scramble current buffer)
nnoremap <F9> mzggg?G`z

" Formatting using Q
nnoremap Q gqip
vnoremap Q gq

" Insert mode completion
inoremap <C-f> <C-x><C-f>
inoremap <C-]> <C-x><C-]>
inoremap <C-l> <C-x><C-l>

" My plugins stuff
nnoremap <leader>r  :RotateScheduleWord<Cr>
nnoremap <silent> <C-y> :call ToggleConceal()<CR>
nnoremap <silent> <leader>tn :ThesisNotes<CR>

" List (quickfix and loclist) navigation
nnoremap <left> :cprev<CR>zvzz
nnoremap <right> :cnext<CR>zvzz
nnoremap <up> :lprev<CR>zvzz
nnoremap <down> :lnext<CR>zvzz

nnoremap <leader>vv :vimgrep // *.<LEFT><LEFT><LEFT><LEFT>
inoremap <leader>vv <ESC>:vimgrep // *.<LEFT><LEFT><LEFT><LEFT>
