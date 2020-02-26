function! CustomFoldText()
    let curline = getline(v:foldstart)
    return curline . "…" .repeat(" ", ActualWindowWidth() - len(curline)-1)
endfunction

