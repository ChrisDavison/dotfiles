setlocal foldmethod=indent

if exists('b:undo_ftplugin')
    let b:undo_ftplugin .= '|foldmethod<'
else
    let b:undo_ftplugin = '|foldmethod<'
endif

let g:pymode_python = 'python3'
let g:slime_paste_file=tempname()
let g:slime_python_ipython = 1
let g:slime_target = "tmux"
if !has('win32')
    let g:lsc_server_commands = {'python': 'pyls'}
endif

" Abbreviations
iab impd import pandas as pd
iab imnp import numpy as np
iab imsp import scipy as sp
iab ifmain if __name__ == "__main__":<CR>    main()
