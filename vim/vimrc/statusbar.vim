" Airline --- {{{

let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_skip_empty_sections=1
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#tabline#left_sep=' '
let g:airline#extensions#tabline#left_alt_sep='|'

let g:airline_section_x = ''
let g:airline_section_y = '%{airline#util#prepend(airline#extensions#tagbar#currenttag(), 0)}%{airline#util#wrap(airline#parts#filetype(), 0)}'
let g:airline_section_z = '%3p%% %#__accent_bold#%{g:airline_symbols.linenr}%#__accent_bold#%4l%#__restore__#%#__restore__#:%3v'

let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n' : 'N',
    \ 'i' : 'I',
    \ 'R' : 'R',
    \ 'c' : 'C',
    \ 'v' : 'V',
    \ 'V' : 'V',
    \ '^V' : 'V',
    \ 's' : 'S',
    \ 'S' : 'S',
    \ '^S' : 'S',
\ }

let g:airline_theme='lucius'
" }}}

" Vim status bar {{{
" %< Where to truncate
" %n buffer number
" %F Full path
" %m Modified flag: [+], [-]
" %r Readonly flag: [RO]
" %y Type:          [vim]
" fugitive#statusline()
" %= Separator
" %-14.(...)
" %l Line
" %c Column
" %V Virtual column
" %P Percentage
" %#HighlightGroup#
if 0
    set statusline=%<[%n]\ %F\ %m%r%y\ %{exists('g:loaded_fugitive')?fugitive#statusline():''}\ %=%-14.(%l,%c%V%)\ %P
    silent! if emoji#available()
      let s:ft_emoji = map({
        \ 'c':          'baby_chick',
        \ 'clojure':    'lollipop',
        \ 'coffee':     'coffee',
        \ 'cpp':        'chicken',
        \ 'css':        'art',
        \ 'eruby':      'ring',
        \ 'gitcommit':  'soon',
        \ 'haml':       'hammer',
        \ 'help':       'angel',
        \ 'html':       'herb',
        \ 'java':       'older_man',
        \ 'javascript': 'monkey',
        \ 'make':       'seedling',
        \ 'markdown':   'book',
        \ 'perl':       'camel',
        \ 'python':     'snake',
        \ 'ruby':       'gem',
        \ 'scala':      'barber',
        \ 'sh':         'shell',
        \ 'slim':       'dancer',
        \ 'text':       'books',
        \ 'vim':        'poop',
        \ 'vim-plug':   'electric_plug',
        \ 'yaml':       'yum',
        \ 'yaml.jinja': 'yum'
        \ }, 'emoji#for(v:val)')

        function! S_filetype()
        if empty(&filetype)
          return emoji#for('grey_question')
        else
          return get(s:ft_emoji, &filetype, '['.&filetype.']')
        endif
      endfunction

      function! S_modified()
        if &modified
          return emoji#for('kiss').' '
        elseif !&modifiable
          return emoji#for('construction').' '
        else
          return ''
        endif
      endfunction

      function! S_fugitive()
        if !exists('g:loaded_fugitive')
          return ''
        endif
        let head = fugitive#head()
        if empty(head)
          return ''
        else
          return head == 'master' ? emoji#for('crown') : emoji#for('dango').'='.head
        endif
      endfunction

      let s:braille = split('"⠉⠒⠤⣀', '\zs')
      function! Braille()
        let len = len(s:braille)
        let [cur, max] = [line('.'), line('$')]
        let pos  = min([len * (cur - 1) / max([1, max - 1]), len - 1])
        return s:braille[pos]
      endfunction

      hi def link User1 TablineFill
      let s:cherry = emoji#for('cherry_blossom')
      function! MyStatusLine()
        let mod = '%{S_modified()}'
        let ro  = "%{&readonly ? emoji#for('lock') . ' ' : ''}"
        let ft  = '%{S_filetype()}'
        let fug = ' %{S_fugitive()}'
        let sep = ' %= '
        let pos = ' %l,%c%V '
        let pct = ' %P '

        return s:cherry.' [%n] %F %<'.mod.ro.ft.fug.sep.pos.'%{Braille()}%*'.pct.s:cherry
      endfunction

      " Note that the "%!" expression is evaluated in the context of the
      " current window and buffer, while %{} items are evaluated in the
      " context of the window that the statusline belongs to.
      set statusline=%!MyStatusLine()
    endif
endif

" }}}
