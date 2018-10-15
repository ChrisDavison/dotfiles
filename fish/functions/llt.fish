function llt
    if test -f (which exa)
       and test -x exa
        exa --group-directories-first --long --tree --git-ignore $args
    end
end
