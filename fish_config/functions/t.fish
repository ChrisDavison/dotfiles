function t
    # Highlight either a date (d=digit: dddd-dd-dd), or a keyword (+WORD)
	command t $argv | rg --passthru "\+\w+|\b\d\d\d\d-\d\d-\d\d\b"
end
