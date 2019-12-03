function tags
	git ls-files | ctags -R --links=no -L-
end
