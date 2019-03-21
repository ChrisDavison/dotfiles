" =====[ Mostly stuff from FZF for navigating buffers ]=====
nnoremap <leader>b :ls<Cr>:b
nnoremap <leader>p :Files<Cr>
nnoremap <leader>g :GFiles<Cr>

" =====[ MY commands ]=====
nnoremap <M-r>      :RotateScheduleWord<Cr>
inoremap <M-r>      <C-o>:RotateScheduleWord<Cr>
nnoremap <C-y>      :call ToggleConceal()<CR>

" =====[ Edit files and source vimrc ]=====
nnoremap <leader>ev :edit ~/src/github.com/chrisdavison/dotfiles/.vimrc<CR>
nnoremap <leader>en :edit ~/Dropbox/notes/**/
nnoremap <leader>ep :edit ~/src/github.com/chrisdavison/dotfiles/vim/plugin/
nnoremap <leader>s  :filt  ls<LEFT><LEFT><LEFT>

" =====[ Insert timestamps ]=====
nnoremap <F5>   :=strftime('%H:%M:%S')<CR>P
inoremap <F5>   <C-R>=strftime('%H:%M:%S')<CR>
nnoremap <F6>   :=strftime('%Y-%m-%d')<CR>P
inoremap <F6>   <C-R>=strftime('%Y-%m-%d')<CR>
nnoremap <F7>   :=strftime('`%Y-%m-%d`')<CR>P
inoremap <F7>   <C-R>=strftime('`%Y-%m-%d`')<CR>

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

