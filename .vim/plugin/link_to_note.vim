function! s:FirstLineFromFileAsLink(filename)
    let title=trim(system('head -n1 ' . a:filename))
    let matches = matchlist(title, '#\+ \(.*\)')
    if len(l:matches) > 1
        let l:title = l:matches[1]
    endif
    let filename=resolve(expand(a:filename))
    if l:filename[0] != '.'
        let filename = './' . a:filename
    endif
    let link="[" . title . "](" . a:filename . ")"
    exec "normal a" . l:link
endfunction

command! -complete=file -nargs=1 InsertLinkToNote call <SID>FirstLineFromFileAsLink(<q-args>)

