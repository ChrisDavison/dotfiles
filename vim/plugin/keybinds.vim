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
nnoremap <leader>ev :silent! exec 'e '.globpath(&rtp, '.vimrc')<BAR>echo "Editing VIMRC"<CR>
nnoremap <leader>ep :EditPlugin 
nnoremap <leader>lp :exec 'Sexplore '.globpath(&rtp, 'vim/plugin')<CR>
nnoremap <leader>sv :exec 'so '.globpath(&rtp, '.vimrc')<BAR>echo "Sourced VIMRC"<CR>
nnoremap <leader>ss :mksession! ~/Dropbox/session.vim<BAR>echo "Saved session to dropbox"<CR>
nnoremap <Leader>hh :set list!<BAR>echo "Toggle hidden characters"<CR>
nnoremap nw :set wrap!<BAR>echo "Toggling line wrapping"<CR>
nnoremap <BS> <C-^>

" My plugins stuff
nnoremap <leader>r  :RotateScheduleWord<Cr>
nnoremap <silent> <C-y> :call ToggleConceal()<CR>
nnoremap <silent> <leader>tn :ThesisNotes<CR>

function! EditRTPPlugin(fname)
    if filereadable(a:fname)
        exec 'edit ' a:fname
    else
        finish
    endif
endfunction

function! GetRTPPlugins(A, L, P)
    let myList = split(globpath(&rtp, 'vim/plugin/*'.a:A.'*.vim'), '\n')
    return filter(myList, 'v:val =~ ".*'. a:A .'.*"')
endfunction

command! -nargs=1 -complete=customlist,GetRTPPlugins EditPlugin call EditRTPPlugin(<f-args>)
