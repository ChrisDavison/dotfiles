function getcost
	awk -F' ' '/cost/{sum+=$2} END{print sum}' (tagsearch $argv)
end
