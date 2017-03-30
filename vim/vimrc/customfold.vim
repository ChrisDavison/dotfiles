" Custom fold ----- {{{1
function! ToggleFold()
    if &foldlevel < 10
        set foldlevel=99
    else
        set foldlevel=0
    endif
endfunction

noremap zt :call ToggleFold()<CR>


function! CustomFoldText()
     "get first non-blank line
     let fs = v:foldstart
     while getline(fs) =~ '^\s*$' | let fs = nextnonblank(fs + 1)
     endwhile
     if fs > v:foldend
         let line = getline(v:foldstart)
     else
         let line = substitute(getline(fs), '\t', repeat(' ', &tabstop), 'g')
     endif

     let w = winwidth(0) - &foldcolumn - (&number ? 6 : 0)
     let foldSize = 1 + v:foldend - v:foldstart
     let foldSizeStr = " [" . foldSize . " lines] "
     let foldLevelStr = repeat("<", v:foldlevel)
     let lineCount = line("$")
     let expansionString = repeat(" ", w - strwidth(foldSizeStr.line.foldLevelStr))
     return line . expansionString . foldSizeStr . foldLevelStr
 endfunction
 set foldnestmax=99
 set foldtext=CustomFoldText()


