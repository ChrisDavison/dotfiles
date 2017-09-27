#! /usr/bin/env bash
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