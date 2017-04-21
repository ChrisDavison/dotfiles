fu! ToggleWrap()
    let wr=&wrap
    if wr
        set nowrap
    else
        set wrap
    endif
endfu

nmap nw :call ToggleWrap()<CR>

fu! CopyFilename()
    let @+=expand("%")
endfu

nnoremap fmt :normal "ggVG="<Cr>
