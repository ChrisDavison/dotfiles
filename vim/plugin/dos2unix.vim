function! Dos2unixFunction()
    let _s=@/
    let l = line(".")
    let c = col(".")
    try
        set ff=unix
        w!
        "%s/\%x0d$//e
    catch /E32:/
        echo "Sorry, first save the file."
    endtry
    let @/=_s
    call cursor(l, c)
endfunction

command! Dos2Unix keepjumps call Dos2unixFunction()
