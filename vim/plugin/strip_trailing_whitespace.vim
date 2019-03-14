if exists('g:loaded_striptrailingwhitespace')
    finish
endif
let g:loaded_striptrailingwhitespace = 1

function StripTrailingWhitespace()
  if !&binary && &filetype != 'diff'
    normal mz
    normal Hmy
    %s/\s\+$//e
    normal 'yz
    normal `z
  endif
endfunction
