function swap
	mv "$argv[2]" "$argv[1].swapping"
and mv "$argv[1]" "$argv[2]"
and mv "$argv[1].swapping" "$argv[1]"
end
