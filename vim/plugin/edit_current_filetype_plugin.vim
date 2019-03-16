" Open, or create, the current file in my dotfiles ftplugins
function! s:EditFtPlugin()
    let fn=finddir('vim\ftplugin\', &rtp) . '\' . &filetype . '.vim'
    exec "edit ".fn
endfunction

command! FTplugin call <SID>EditFtPlugin()

