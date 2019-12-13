function fcoc --description 'Fuzzy checkout git commits'
	set commit (git log --pretty=oneline --abbrev-commit --reverse | fzf --tac +s +m -e)
git checkout (echo "$commit" | sed "s/ .*//")
end
