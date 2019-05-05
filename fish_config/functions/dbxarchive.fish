function dbxarchive
	set -l archivedir $HOME/Dropbox/archive
if test -d "$archivedir"
echo "Moving $argv to Dropbox/archive"
mv $argv "$archivedir"
else
echo "Couldn't find dropbox archive directory: $archivedir"
end
end
