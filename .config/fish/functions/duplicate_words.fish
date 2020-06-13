function duplicate_words
    if [ (count $argv) -eq 0 ]
        echo "usage: duplicate_words <file>..."
        return
    end
    grep -Eo '(\b.+) \1\b' $1; and true
end

