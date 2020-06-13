function monospace-fonts
    fc-list :mono | cut -d':' -f2  | cut -d',' -f1 | sort | uniq
end
