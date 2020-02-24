function CopyFilenameAsMarkdownLink()
    let link=s:make_markdown_link(expand('%'), "./" . expand('%'))
    let @a=l:link
endfunction

function! s:make_markdown_link(text, url)
    return "[" . a:text . "](" . a:url . ")"
endfunction
