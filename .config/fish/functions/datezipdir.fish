function datezipdir
    if [ (count $argv) -eq 0 ]
        echo "usage: datezipdir <directory>"
        return
    end
    set dirname (basename $argv[1])
    zip -r (date +"$dirname--%Y-%m-%d.zip") $argv[1]
end

