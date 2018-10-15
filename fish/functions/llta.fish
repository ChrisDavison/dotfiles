function llta
    if test -f (which exa)
       and test -x exa
        exa --group-directories-first --long --tree $args
    end
end
