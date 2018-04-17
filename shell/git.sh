#! /usr/bin/env bash
# Nice git aliases
alias g='git'

# Mixu git-run (gr) aliases...
# For handling multiple git repos
alias dr='gr status | grep -E "behind|ahead|modified"'
alias gitsync='gr git fetch --all'
alias gitdown='gr git pull --rebase'

mygitinit() {
    git init
    touch .gitignore
    echo ".gitignore" > .gitignore
    touch README.md
    git add README.md
    git commit -m "Initial Commit"
}

git_merge() {
    proj=$1
    gitdir=$2
    git remote add -f $proj $gitdir
    git merge -s ours --allow-unrelated-histories --no-commit $proj/master
    git read-tree --prefix=$proj/ -u $proj/master
    echo "Need to commit"
}

gbetween() {
    git plgdall --after="$1" --before="$2"
}

gsince() {
    git plgdall --after="$1"
}
