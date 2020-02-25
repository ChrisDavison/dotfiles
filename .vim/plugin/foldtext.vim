function! CustomFoldText()
    let lines_count_text = printf("‖ %4S ‖", v:foldend - v:foldstart)
    let curline = getline(v:foldstart)
    let len_text = len(curline) + len(l:lines_count_text)
    let padding = repeat(" ", ActualWindowWidth() - len_text)
    return curline . padding . lines_count_text
endfunction

