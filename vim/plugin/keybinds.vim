" =====[ Mostly stuff from FZF for navigating buffers ]=====
nnoremap <leader>b :Buffers<Cr>
nnoremap <leader>p :Files<Cr>
nnoremap <leader>g :GFiles<Cr>

" =====[ MY commands ]=====
nnoremap <C-r>  :RotateScheduleWord<Cr>
inoremap <C-r>  <C-o>:RotateScheduleWord<Cr>
nnoremap <C-y> :call ToggleConceal()<CR>
nnoremap <leader>tn :ThesisNotes<CR>

" =====[ Edit files and source vimrc ]=====
nnoremap <leader>ev :edit ~/src/github.com/chrisdavison/dotfiles/.vimrc<CR>
nnoremap <leader>en :Files ~/Dropbox/notes<CR>
nnoremap <leader>ep :edit ~/src/github.com/chrisdavison/dotfiles/vim/plugin/
nnoremap <leader>ss :mksession! ~/Dropbox/session.vim<CR>

" =====[ Uppercase the current word (from anywhere within the <word>) ]=====
inoremap <C-u> <esc>mzgUiw`za

" =====[ Panic button (scramble current buffer) ]=====
nnoremap <F9> mzggg?G`z

" =====[ Formatting using Q ]=====
nnoremap Q gqip
vnoremap Q gq

" =====[ List (quickfix and loclist) navigation ]=====
nnoremap <left> :cprev<CR>zvzz
nnoremap <right> :cnext<CR>zvzz
nnoremap <up> :lprev<CR>zvzz
nnoremap <down> :lnext<CR>zvzz

" =====[ Generic useful stuff ]=====
inoremap jj <ESC>
nnoremap <BS> <C-^>
nnoremap S :%s///<LEFT>
vnoremap S :s///<LEFT>
vnoremap < <gv
vnoremap > >gv

