function vi
    if test -x /Applications/Macvim.app/bin/mvim
        command mvim -v $argv
    else
        command vi $argv
    end
end

