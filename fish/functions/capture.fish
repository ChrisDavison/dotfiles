function capture
    set -l d (date +"%F %T")
    if test -f "$CAPTUREFILE"
        echo "- $d $argv" >> "$CAPTUREFILE"
    else
        echo "CAPTUREFILE not defined"
    end
end

