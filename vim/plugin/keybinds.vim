" =====[ Edit files and source vimrc ]=====
nnoremap <leader>ev :edit ~/code/dotfiles/.vimrc<CR>
nnoremap <leader>en :edit ~/Dropbox/notes/**/*
nnoremap <leader>ep :edit ~/code/dotfiles/vim/plugin/
nnoremap <leader>b :ls<Cr>:b
nnoremap <leader>s  :ls<CR>:filt  ls<LEFT><LEFT><LEFT>

" =====[ Uppercase the current word (from anywhere within the <word>) ]=====
inoremap <C-u>   <esc>mzgUiw`za

" =====[ Panic button (scramble current buffer) ]=====
nnoremap <F9>    mzggg?G`z

" =====[ List (quickfix and loclist) navigation ]=====
nnoremap <left>  :cprev<CR>zvzz
nnoremap <right> :cnext<CR>zvzz
nnoremap <up>    :lprev<CR>zvzz
nnoremap <down>  :lnext<CR>zvzz

" =====[ Generic useful stuff ]=====
inoremap jj     <ESC>:w<CR>
nnoremap <BS>   <C-^>
nnoremap S      :%s///<LEFT>
vnoremap S      :s///<LEFT>
vnoremap <      <gv
vnoremap >      >gv

" =====[ MY commands ]=====
nnoremap <M-r>      :RotateScheduleWord<Cr>
inoremap <M-r>      <C-o>:RotateScheduleWord<Cr>
nnoremap <C-y>      :call ToggleConceal()<CR>
