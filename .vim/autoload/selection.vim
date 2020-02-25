function! selection#visual(only_on_line)
    let l:start_line = line("'<")
    let l:start_col = col("'<")
    let l:end_line = line("'>")
    let l:end_col = col("'>")
    if a:only_on_line && (l:start_line != l:end_line)
        echom "FileFromSelected: Start and end must be same line number"
        return
    end
    return getline(".")[l:start_col-1:l:end_col-1]
endfunction

function! selection#before_and_after_visual()
    let start_line = line("'<")
    let start_col = col("'<")
    let end_line = line("'>")
    let end_col = col("'>")
    let before=getline(start_line)[:start_col-2]
    if start_col == 1
        let before = ""
    end
    let after=getline(start_line)[end_col:]
    return [before, after]
endfunction
