function ActualWindowWidth()
    return winwidth(0) - s:NumberColumnWidth() - &foldcolumn - s:SignsWidth()
endfunction

function s:SignsWidth()
    let l:signs_width = 0
    if has('signs')
        " This seems to be the only way to find out if the signs column is even
        " showing.
        let l:signs = []
        let l:signs_string = ''
        redir =>l:signs_string|exe "sil sign place buffer=".bufnr('')|redir end
        let l:signs = split(l:signs_string, "\n")[1:]

        if !empty(signs)
            let l:signs_width = 2
        endif
    endif

    return l:signs_width
endfunction

function s:NumberColumnWidth()
    let l:number_col_width = 0
    if &number
        let l:number_col_width = max([strlen(line('$')) + 1, 3])
    elseif &relativenumber
        let l:number_col_width = 3
    endif

    if l:number_col_width != 0
        let l:number_col_width = max([l:number_col_width, &numberwidth])
    endif

    return l:number_col_width
endfunction
