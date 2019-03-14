if executable('rg')
    set grepprg=rg\ --vimgrep
    " let s:find_cmd=
    command! -bang -nargs=* Find call fzf#vim#grep(
    \    'rg --no-heading -F --smart-case --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
    nnoremap <leader>F :Find<SPACE>
endif
