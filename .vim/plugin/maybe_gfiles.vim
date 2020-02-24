function MaybeGFiles()
    " system is only called to test for it's error code
    call system('git rev-parse --show-toplevel')
    if !v:shell_error
        GFiles
    else 
        Files
    endif
endfunction

