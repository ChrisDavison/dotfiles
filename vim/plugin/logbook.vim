function! CurrentLogbook()
    let logbooks=globpath(expand("~/Dropbox/notes/logbook"), "*.md", 0, 1)
    let last_logbook=get(logbooks, len(logbooks)-1)
    exec ":e ".last_logbook | normal G
endfunction
command! Logbook exec CurrentLogbook() | normal Gzz
