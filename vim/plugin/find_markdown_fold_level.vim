function! MarkdownLevel()
    let h = matchstr(getline(v:lnum), '^#\+')
    if empty(h)
        return "="
    endif
    return ">" . len(h)
endfunction
