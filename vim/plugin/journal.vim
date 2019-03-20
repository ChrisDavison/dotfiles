function! CurrentJournal()
    let journals=globpath(expand("~/Dropbox/notes/journal"), "*.md", 0, 1)
    let last_journal=get(journals, len(journals)-1)
    exec ":e ".last_journal | normal G
endfunction
command! Journal exec CurrentJournal()
