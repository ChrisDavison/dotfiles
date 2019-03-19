if exists('g:schedule_loaded')
    finish
endif
let g:schedule_loaded = 1

let g:schedule_words = [ 'TODO' , 'WAITING', 'DONE', 'CANCELLED' ]
function! RotateWord()
    let N = len(g:schedule_words)
    for word in g:schedule_words
        let coln = match(getline(line(".")), word)
        if coln > 0
            let idx = index(g:schedule_words, word)
            let next = g:schedule_words[(idx+1) % N]
                        call cursor(0, coln+1)
            exec "normal ciw" . next 
            exec "normal $"
            return
        endif
    endfor
endfunction
command! RotateScheduleWord call RotateWord()
