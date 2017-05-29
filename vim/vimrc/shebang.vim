" Automatically add HashBang lines ----- {{{1
function! Hashbang(portable, permission)
let shells = {
        \    'awk': "awk",
        \     'sh': "bash",
        \     'hs': "runhaskell",
        \     'jl': "julia",
        \    'lua': "lua",
        \    'mak': "make",
        \     'js': "node",
        \      'm': "octave",
        \     'pl': "perl",
        \    'php': "php",
        \     'py': "python",
        \      'r': "Rscript",
        \     'rb': "ruby",
        \  'scala': "scala",
        \    'tcl': "tclsh",
        \    ' tk': "wish"
        \    }

let extension = expand("%:e")

if has_key(shells,extension)
    let fileshell = shells[extension]

    if a:portable
        let line =  "#! /usr/bin/env " . fileshell
    else
        let line = "#! " . system("which " . fileshell)
    endif

    0put = line

    if a:permission
        :autocmd BufWritePost * :autocmd VimLeave * :!chmod u+x %
    endif

endif

endfunction

autocmd! BufNewFile *.* :call Hashbang(1,1)


