" Open, or create, the current file in my dotfiles ftplugins
function! s:EditFtPlugin()
    let fn=finddir('vim/ftplugin/', &rtp) . '/' . &filetype . '.vim'
    exec "edit ".fn
endfunction

command! FTplugin call <SID>EditFtPlugin()

function! EditRTPPlugin(fname)
    if filereadable(a:fname)
        exec 'edit ' a:fname
    else
        finish
    endif
endfunction

function! GetRTPPlugins(A, L, P)
    let myList = split(globpath(&rtp, 'vim/plugin/*'.a:A.'*.vim'), '\n')
    return filter(myList, 'v:val =~ ".*'. a:A .'.*"')
endfunction

command! -nargs=1 -complete=customlist,GetRTPPlugins Eplug call EditRTPPlugin(<f-args>)
