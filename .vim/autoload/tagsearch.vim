function! s:make_list(fn, pattern)
    let tags=join(map(split(a:pattern, ' '), {_, val -> '@' . val}), ' ')
    return { 'filename': fnamemodify(a:fn, ':p'), 'text': l:tags,
           \ 'lnum': 1, 'col': 0,
           \ }
endfunction

function! tagsearch#untagged()
    let files=systemlist('tagsearch -u')
    let filedict=map(l:files, {_, val -> <sid>make_list(val, 'untagged')})
    call setloclist(0, l:filedict)
endfunction

function! tagsearch#untagged_fzf()
    call fzf#run({'source': 'tagsearch -u', 'sink': 'e', 'down': '30%'})
endfunction

function! tagsearch#list(tags)
    let files=systemlist('tagsearch ' . a:tags)
    let filedict=map(l:files, {_, val -> <sid>make_list(val, a:tags)})
    call setloclist(0, l:filedict)
endfunction

function! tagsearch#list_fzf(tags)
    call fzf#run({'source': 'tagsearch ' . a:tags, 'sink': 'e', 'down': '30%'})
endfunction

command! TagsearchUntagged call tagsearch#untagged()|lopen
command! TagsearchUntaggedFZF call tagsearch#untagged_fzf()
command! TSU call tagsearch#untagged()|lopen
command! TSUF call tagsearch#untagged_fzf()

command! -nargs=+ TagsearchList call tagsearch#list(<q-args>)|lopen
command! -nargs=+ TagsearchListFZF call tagsearch#list_fzf(<q-args>)
command! -nargs=+ TSL call tagsearch#list(<q-args>)|lopen
command! -nargs=+ TSLF call tagsearch#list_fzf(<q-args>)

