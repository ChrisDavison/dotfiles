function asmrartists
	cat $ASMRFILE | cut -d':' -f1 | tr -d '+' | sort | uniq | tr "\n" "," | sed -e "s/,/.../g"
end
