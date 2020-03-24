au BufNewFile,BufFilePre,BufRead todo.txt,done.txt,habits.txt,report.txt,thesis.txt set filetype=todo.txt
set formatoptions-=a

command! TodoSort call todo#Sort('')
command! TodoSortDue call todo#SortDue()
command! TodoSortCP call todo#HierarchicalSort('@', '+', 1)
command! TodoSortPC call todo#HierarchicalSort('+', '@', 1)
