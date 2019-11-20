function logbook_today
	set context $argv[1]
echo $context
set fn ~/src/github.com/ChrisDavison/logbook/(date +%Y)/(date +"%Y%m%d-")$context.md
pushd ~/src/github.com/ChrisDavison/logbook/
touch $fn
git add --all
git commit -m "Start entry '$context'"
nvim $fn
popd
end
