function mdformatw
	pandoc --to markdown-shortcut_reference_links+pipe_tables-simple_tables --wrap=auto --columns=80 --reference-links --reference-location=section --atx-headers $argv[1] -o $argv[1]
end
