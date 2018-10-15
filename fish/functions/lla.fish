function lla
    if test -f (which exa)
       and test -x exa
        exa --group-directories-first --long $args
    end
end
