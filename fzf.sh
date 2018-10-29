fbr() {
    local branches branch
    branches=$(git branch -vv)
    branch=$(echo "$branches" | fzf +m)
    git checkout $(echo "$branch" | awk {'print $1}' | sed "s/.* //")
}

fco() {
    local tags branches target
    tags=$(
        git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
      branches=$(
        git branch --all | grep -v HEAD             |
        sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
        sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
      target=$(
        (echo "$tags"; echo "$branches") |
        fzf-tmux -- --ansi +m -d "\t" -n 2) || return
      git checkout $(echo "$target" | awk '{print $2}')
 }

# fcoc - checkout git commit
fcoc() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

# fuzzy grep open via ag
vg() {
  local file

  file="$(rg --no-heading $@ | fzf -0 -1 | awk -F: '{print $1}')"

  if [[ -n $file ]]
  then
     vim $file
  fi
}
