function FoldLevelMarkdown()
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
endfunction
