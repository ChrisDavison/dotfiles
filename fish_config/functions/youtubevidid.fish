function youtubevidid
	echo $argv | rg "\?v=(.{11})&" -o -r '$1'
end
