function nfc
    nf $argv | rg "^\(C\)" | cut -d' ' -f2-
end
