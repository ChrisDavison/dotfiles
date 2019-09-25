function sanitise
	set direc (dirname $argv[1])
    set base (basename $argv[1])
    echo "$base" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-zA-Z0-9.-]/-/g' | tr -s - - | sed 's/\-$//g'
end
