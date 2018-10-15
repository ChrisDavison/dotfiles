function lt
    if test -f (which exa)
       and test -x exa
        exa --group-directories-first $args
    end
end
