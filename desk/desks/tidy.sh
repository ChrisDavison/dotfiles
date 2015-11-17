# Description: desk for tidying datasets

# Print rows until a regex is found at the start of a row
ignore_after () {
  regex='/^"$1"/'
  awk '"$regex"{exit} {print $0}' $2
}

# Ignore rows until a regex is found at the start of a row
ignore_before () {
  regex='/^"$1"/'
  awk '"$regex"{start=1} start==1{print $0}' $2
}

# Ignore rows before regex1 is found, exit after regex2 is found.
print_between () {
  regex=/^"$1"/
  regex2=/^"$2"/
  awk '"$regex"{start=1} "$regex2"{exit} start==1{print $0}' $3
}

desk
