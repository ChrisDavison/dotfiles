function! ArchiveJournal() range
    echom "ArchiveJournal Not yet working!"
    return
    let cur=expand('%:p')
    let archive=expand("~/Dropbox/notes/journal-archive.md")
    if cur != expand("~/Dropbox/notes/journal.md")
        echom "Not in journal."
        return
    endif
    let start=line("v")
    let end=line(".")
    normal mZ
    exec ":'<,'>normal D"
    exec ":e ".archive | normal GP
    normal `Z
endfunction
command! -range ArchiveJournal exec ArchiveJournal()

