function! RotateWord()
    let N = len(g:cd_schedule_words)
    for word in g:cd_schedule_words
        let coln = match(getline(line(".")), word)
        if coln > 0
            let idx = index(g:cd_schedule_words, word)
            let next = g:cd_schedule_words[(idx+1) % N]
                        call cursor(0, coln+1)
            exec "normal ciw" . next 
            exec "normal $"
            return
        endif
    endfor
endfunction
command! RotateScheduleWord call RotateWord()
