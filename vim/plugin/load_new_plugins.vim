function! NewPlugins()
    let s:pth = globpath(&rtp, '*/junegunn_plug_plugins.vim')
    exec 'source '.s:pth | PlugInstall
endfunction
