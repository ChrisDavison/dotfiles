function ll
    if test -f (which exa)
       and test -x exa
        exa --group-directories-first --long --git-ignore $args
    end
end
