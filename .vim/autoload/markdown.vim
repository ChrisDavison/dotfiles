function! markdown#filename_as_header()
    let filename=expand('%:t:r')
    let as_header='# ' . <sid>titlecase(substitute(l:filename, '-', ' ', 'g'))
    exec "norm O" . as_header
endfunction

function! s:titlecase(str)
    let words=split(a:str, '\W\+')
    let titled=map(l:words, {_, word -> toupper(word[0]) . word[1:]})
    return join(l:titled, ' ')
endfunction


function! markdown#goto_file(split)
    let fname=expand("<cfile>")
    let command = "edit "
    if a:split > 0
        if winwidth(0) > 160
            let command = "vsplit "
        else
            let command = "split "
        endif
    endif
    if filereadable(l:fname)
        execute "silent!" . l:command . l:fname
    else
        if getline(".")[col(".")] != "]"
            normal f]
        end
        normal vi("by
        if filereadable(getreg("b"))
            execute "silent!" . l:command . getreg("b")
        else
            echom "Couldn't find valid link."
        end
    end
endfunction 

function! markdown#backlinks(use_grep)
    if a:use_grep
        exec "silent grep! '\\((\./)*" . expand("%") . "'"
        if len(getqflist()) == 0
            exec "cclose"
        endif
    else
        call fzf#vim#grep(
        \ "rg --column --line-number --no-heading --color=always --smart-case -g '!tags' ".expand('%'), 1,
        \ fzf#vim#with_preview('right:50%:hidden', '?'), 0)
    end
endfunction

function! markdown#move_visual_selection_to_file(start, end)
    let filename=input("Filename: ")
    let filename_nospace=substitute(l:filename, ' ', '-', 'g')
    let linequery=a:start . "," . a:end
    silent! exec ":" . l:linequery . "w " . l:filename_nospace . ".md"
    silent! exec ":" . l:linequery . "d"
endfunction

function! markdown#new_section()
    call append(line('$'), ['', '## '])
    exec 'norm G'
    startinsert!
endfunction

function! markdown#header_increase()
    let save_cursor = getcurpos()
    exec "silent %s/^\\(#\\+\\)/\\1#/"
    call setpos('.', l:save_cursor)
endfunction

function! markdown#header_decrease()
    let save_cursor = getcurpos()
    exec "silent %s/^\\(#\\+\\)#/\\1/"
    call setpos('.', l:save_cursor)
endfunction
