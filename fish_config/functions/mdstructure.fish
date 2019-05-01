function mdstructure
	set -l cmd $argv[1]
    set -l files $argv[2..(count $argv)]
    set -l query ""
    switch $cmd
        case links url urls
            set query "[^!]\[.*?\]\(/*?\)"
        case images img
            set query "!\[.*?\]\(.*?\)"
        case keywords hashtags
            set query "(?:[\s\`^])#[a-zA-Z]+"
        case headers headings
            set query "^#+ .*"
        case '*'
            echo "Unrecognised command: $cmd"
            echo "links, images, keywords, or headers"
            return 1
    end
    rg "$query" $files -g -o --no-heading --sort=path
end
