au BufNewFile,BufFilePre,BufRead todo.txt,done.txt set filetype=todo.txt
let maplocalleader='<SPACE>'
set formatoptions-=a

command! TodoSort call todo#Sort('')
command! TodoSortDue call todo#SortDue()
command! TodoSortCP call todo#HierarchicalSort('@', '+', 1)
command! TodoSortPC call todo#HierarchicalSort('+', '@', 1)
