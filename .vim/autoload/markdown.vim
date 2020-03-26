function! markdown#fold_level() " {{{1
    let matches_atx = matchlist(getline(v:lnum), '^\(#\+\)\s')
    let line_len = len(getline(v:lnum))
    let matches_setex_one = len(matchlist(getline(v:lnum+1), '^=\+$')) > 0
    let matches_setex_two = len(matchlist(getline(v:lnum+1), '^-\+$')) > 0
    let prev_not_blank = len(getline(v:lnum)) > 0
    if len(l:matches_atx) > 0 
        if g:markdown_fold_method == 'stacked'
            return ">1"
        else
            return ">" . len(l:matches_atx[1])
        end
    elseif l:matches_setex_one && prev_not_blank
        return ">1"
    elseif l:matches_setex_two && prev_not_blank
        if g:markdown_fold_method == 'stacked'
            return ">1"
        else
            return ">2"
        endif
    else
        return "="
    end
endfunction " }}}1

function! markdown#goto_file(split) " {{{1
    let fname=expand("<cfile>")
    if a:split == 2
        let command = "split "
    elseif a:split == 1
        let command = "vsplit "
    else
        let command = "edit "
    endif
    if filereadable(l:fname)
        execute "silent!" . l:command . l:fname
    else
        if getline(".")[col(".")] != "]"
            normal f]
        end
        normal vi("by
        if filereadable(getreg("b"))
            execute "silent!" . l:command . getreg("b")
        else
            echom "Couldn't find valid link."
        end
    end
endfunction " }}}1

function! markdown#backlinks(use_grep) " {{{1
    if a:use_grep
        normal mZ
        exec "grep " . expand("%")
        normal `Z
    else
        call fzf#vim#grep(
        \ "rg --column --line-number --no-heading --color=always --smart-case -g '!tags' ".expand('%'), 1,
        \ fzf#vim#with_preview('right:50%:hidden', '?'), 0)
    end
endfunction " }}}1

function! markdown#copy_filename_as_link() " {{{1
    let link=s:make_markdown_link(expand('%'), "./" . expand('%'))
    let @a=l:link
endfunction " }}}1

function! s:make_markdown_link(text, url) " {{{1
    return "[" . a:text . "](" . a:url . ")"
endfunction " }}}1

function! markdown#file_from_selection(is_visual) " {{{1
    let text= a:is_visual ? selection#visual(1) : expand('<cword>')
    let l:start_line = line(".")
    let l:start_col = col(".")
    let linktext="./" . sanitise#filename(l:text) . ".md"
    let replacetext=s:make_markdown_link(l:text, linktext)
    if a:is_visual
        let around_visual = selection#before_and_after_visual()
        let l:line=around_visual[0] . replacetext . around_visual[1]
        call setline(l:start_line, l:line)
    else
        execute "normal ciw" . l:replacetext
    end
    call cursor(l:start_line, l:start_col+1)
    return linktext
endfunction " }}}1

function! markdown#file_from_selection_and_edit(is_visual) " {{{1
    exec "w|edit " . markdown#file_from_selection(a:is_visual)
endfunction " }}}1
