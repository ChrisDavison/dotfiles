function SanitiseFilename(filename)
    let nospace = substitute(a:filename, " ", "-", "g")
    let lower = tolower(nospace)
    let nosyms = substitute(lower, "[^a-zA-Z0-9\-]", "", "g")
    return nosyms
endfunction
