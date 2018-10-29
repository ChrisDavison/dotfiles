fbr() { # FZF through git branches
    local branches branch
    branches=$(git branch -vv)
    branch=$(echo "$branches" | fzf +m)
    if [[ -n "$branch" ]]; then
        git checkout $(echo "$branch" | awk {'print $1}' | sed "s/.* //")
    else
        echo "No branch given"
    fi
}

fco() { # FZF through branches and tags
    local tags branches target
    tags=$( git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}' ) || return
    branches=$(
      git branch --all | grep -v HEAD             |
      sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
      sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
    target=$(
      (echo "$tags"; echo "$branches") |
      fzf-tmux -- --ansi +m -d "\t" -n 2) || return
    git checkout $(echo "$target" | awk '{print $2}')
 }

fcoc() { # FZF through git commits
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

fshow(){ # FZF and show a commit
    git show $(git log --pretty=oneline | fzf -q "$1" | cut -d=' ' -f1)
}

vg() { # FZF through grep results, and open (multiple, using tab) files in vim
  local file

  file="$(rg --no-heading $@ | fzf -0 -1 -m | awk -F: '{print $1}')"

  if [[ -n $file ]]
  then
     vim $file
  fi
}

bm(){ # Jump to 'bookmark' directories (dirs in ~/.bm)
    local selected
    selected=$(cat ~/.bm | sed "s/#.*//g" | sed '/^\s*$/d' | fzf -1 -q "$1")
    if [[ -n $selected ]]; then
        cd $selected
    fi
}

add_bm(){
    echo $(pwd) >> ~/.bm
    cat ~/.bm | sort | uniq > ~/.bm.bak
    mv ~/.bm.bak ~/.bm
}

fop(){
    open $(fzf -m -q "$1")
}
