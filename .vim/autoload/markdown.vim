function! markdown#filename_as_header()
    let filename=expand('%:t:r')
    let as_header='# ' . substitute(l:filename, '-', ' ', 'g')
    exec "norm O" . as_header
endfunction
command! FilenameAsHeader call markdown#filename_as_header()

