function datezipdir
	set dirname (basename $argv[1])
set zipname (date +"$dirname--%Y-%m-%d.zip")
echo $zipname
zip -r $zipname $argv[1]
end
