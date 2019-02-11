function t
	command t $argv | rg --passthru "\+\w+|due:.*\b"
end
