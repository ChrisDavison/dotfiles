autocmd BufNewFile,BufReadPost *.md set filetype=markdown

let g:markdown_fenced_languages = ['c', 'cpp', 'coffee', 'css', 'erb=eruby', 'javascript', 'js=javascript', 'json=javascript', 'ruby', 'sass', 'xml', 'html', 'python', 'py=python', 'rust', 'racket', 'go']
let g:scratch_filetype = 'markdown'


autocmd BufNewFile,BufReadPost *.tex set filetype=tex
autocmd FileType c      set foldmethod=syntax
autocmd FileType python set foldmethod=indent
autocmd FileType python set tabstop=4
autocmd FileType python set softtabstop=4
autocmd FileType go     set nofen
autocmd FileType make   set noexpandtab
autocmd FileType rust   set foldmethod=syntax
autocmd FileType vim    set foldmethod=marker
autocmd BufNewFile,BufReadPost *.es6 set filetype=javascript

let g:pandoc#spell#enabled=0
let b:javascript_fold=1


