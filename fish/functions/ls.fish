function ls
    if test -f (which exa)
       and test -x exa
        exa --group-directories-first --git-ignore $args
    end
end
