function append_to_path
    if test -d $argv[1]
       and not string match $argv[1] $PATH
        set PATH $argv[1] $PATH
    end
end
