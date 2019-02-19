function t
	command t $argv | rg --passthru "!|\+\w+|\b\d\d\d\d-\d\d-\d\d\b"
end
