function! RotateWord()
    let N = len(g:cd_schedule_words)
    let cur = substitute(expand('<cWORD>'), '\**', '', 'g')
    let idx = index(g:cd_schedule_words, cur)
    if idx >= 0
        let next = g:cd_schedule_words[(idx+1) % N]
        let cmd = "ciW**" . next . "**"
        execute "normal " . cmd
    endif
endfunction
command! RotateScheduleWord call RotateWord()
