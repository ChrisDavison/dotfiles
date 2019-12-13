function fbr --description 'Checkout git branches (MRU order) using FZF'
	set branches (git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)" | grep -v HEAD)
set branch (echo $branches | fzf)
git checkout (echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
end
