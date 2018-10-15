function lta
    if test -f (which exa)
        and test -x exa
        exa --tree -L 2 $args
    end
end
