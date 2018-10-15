function git-root --description 'CD to root of git repo'
	cd (git rev-parse --show-toplevel)
end
