function colourtoggle#time()
    if strftime("%H") >= 21 || strftime("%H") < 8
        call colourtoggle#dark()
    else
        call colourtoggle#light()
    end
endfunction

function colourtoggle#toggle()
    if &bg == "dark"
        call colourtoggle#light()
    else
        call colourtoggle#dark()
    endif
endfunction

function colourtoggle#dark()
    " colorscheme corvine
    exec "colorscheme " . g:dark_scheme
    set bg=dark
endfunction

function colourtoggle#light()
    " colorscheme corvine_light
    exec "colorscheme " . g:light_scheme
    set bg=light
endfunction

