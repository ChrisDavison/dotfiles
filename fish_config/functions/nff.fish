function nff
    nf $argv | rg "^\(F\)" | cut -d' ' -f2-
end
