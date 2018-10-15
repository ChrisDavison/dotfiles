function append_to_path
    set -l match (string match $argv[1] $PATH > /dev/null)
    if test -d $argv[1]
       and not eval $match
        set -xg PATH $argv[1] $PATH
    end
end
