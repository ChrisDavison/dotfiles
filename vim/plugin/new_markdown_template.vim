" Insert filename as header of new markdown file {{{2
function! VimNewMarkdown(fname)
    exec ":normal 0i# " . substitute(fnamemodify(a:fname, ':t:r:gs/-/ /'), "\\<.", "\\u&", "g")
endfunction
"}}}2
