function nonascii
    rg "[^\x00-\x7F£\p{Greek}]" -o --no-heading
end
