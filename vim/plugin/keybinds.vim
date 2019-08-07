" =====[ Edit files and source vimrc ]=====
nnoremap <leader>ev :edit ~/code/dotfiles/.vimrc<CR>
nnoremap <leader>en :edit ~/Dropbox/notes/**/*
nnoremap <leader>ep :edit ~/code/dotfiles/vim/plugin/
nnoremap <leader>b :ls<Cr>:b
nnoremap <leader>s  :ls<CR>:filt  ls<LEFT><LEFT><LEFT>

if exists('g:loaded_fzf')
    nnoremap <leader>en :Files ~/Dropbox/notes/<CR>
    nnoremap <leader>ep :Files ~/code/dotfiles/vim/plugin<CR>
    nnoremap <leader>b :Buffers<CR>
endif

" =====[ Uppercase the current word (from anywhere within the <word>) ]=====
inoremap <C-u>   <esc>mzgUiw`za

" =====[ Panic button (scramble current buffer) ]=====
nnoremap <F9>    mzggg?G`z

" =====[ List (quickfix and loclist) navigation ]=====
nnoremap <left>  :cprev<CR>zvzz
nnoremap <right> :cnext<CR>zvzz
nnoremap <up>    :lprev<CR>zvzz
nnoremap <down>  :lnext<CR>zvzz

nnoremap <silent> Q =ip

" =====[ Generic useful stuff ]=====
inoremap jj     <ESC>:w<CR>
nnoremap <BS>   <C-^>
nnoremap S      :%s///<LEFT>
vnoremap S      :s///<LEFT>
vnoremap <      <gv
vnoremap >      >gv
nnoremap j      gj
nnoremap k      gk

" =====[ MY commands ]=====
if exists('g:schedule_loaded')
    nnoremap <M-r>      :RotateScheduleWord<Cr>
    inoremap <M-r>      <C-o>:RotateScheduleWord<Cr>
endif

if exists('g:loaded_toggleconceal')
    nnoremap <C-y>      :call ToggleConceal()<CR>
endif
