function fromepoch
	date -r "$argv[1]" +"%Y%m%d %H:%M:%S"
end
