function! FileFromSelected(is_visual)
    let text= a:is_visual ? GetVisualSelection(1) : expand('<cword>')
    let l:start_line = line(".")
    let l:start_col = col(".")
    let linktext="./" . s:sanitise_filename(l:text) . ".txt"
    let replacetext=s:make_markdown_link(l:text, linktext)
    if a:is_visual
        let around_visual = GetTextAroundVisual()
        let l:line=around_visual[0] . replacetext . around_visual[1]
        call setline(l:start_line, l:line)
    else
        execute "normal ciw" . l:replacetext
    end
    call cursor(l:start_line, l:start_col+1)
    return linktext
endfunction
function! EditFileFromSelected(is_visual)
    exec "w|edit " . FileFromSelected(a:is_visual)
endfunction
nnoremap ml :call FileFromSelected(0)<CR>
vnoremap ml :call FileFromSelected(1)<CR>

nnoremap gml :call EditFileFromSelected(0)<CR>
vnoremap gml :call EditFileFromSelected(1)<CR>
