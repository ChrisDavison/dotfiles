function nf
    if [ ! -d "$NOTESDIR" ]
        echo "NOTESDIR not defined"
        return 1
    end
    set loc $NOTESDIR
    set i_start 1
    if [ -d $1 ]
        set loc $1
        set i_start 2
    end

    fd "$argv[$i_start..-1]" "$loc" -e md | sed -e "s/^/(F) /"
    fd "$argv[$i_start..-1]" "$loc" -t d | sed -e "s/^/(D) /"
    rg "$argv[$i_start..-1]" "$loc" -l | sed -e "s/^/(C) /"
end
