function note
	set -l files (notes)
test -z $files; and return 1
vim $files
end
