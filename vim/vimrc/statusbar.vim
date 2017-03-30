" Airline --- {{{1

let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_skip_empty_sections=1
let g:airline#extensions#tabline#enabled=1

let g:airline#extensions#tabline#left_sep=' '
let g:airline#extensions#tabline#left_alt_sep='|'

let g:airline_section_x = ''
let g:airline_section_y = '%{airline#util#prepend(airline#extensions#tagbar#currenttag(), 0)}%{airline#util#wrap(airline#parts#filetype(), 0)}'
let g:airline_section_z = '%3p%% %#__accent_bold#%{g:airline_symbols.linenr}%#__accent_bold#%4l%#__restore__#%#__restore__#:%3v'

let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n' : 'N',
    \ 'i' : 'I',
    \ 'R' : 'R',
    \ 'c' : 'C',
    \ 'v' : 'V',
    \ 'V' : 'V',
    \ '^V' : 'V',
    \ 's' : 'S',
    \ 'S' : 'S',
    \ '^S' : 'S',
\ }

let g:airline_theme='lucius'
