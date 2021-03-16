#!/usr/bin/env bash

USAGE="usage: fzg [cmd]

Do some git stuff with fuzzyfind (fzf).

commands:
    show                fuzzy select commit and checkout
    checkout (co)       fuzzy checkout tag/branch/target
    status (stat|s)     fuzzy select from git status
"

is_in_git_repo() {
    git rev-parse HEAD >/dev/null 2>&1
}

fshow(){ # fuzzy show commit {{{1
    local commit commits
    commits=$(git log --oneline) &&
        commit=$(echo "$commits" | fzf --preview 'git show --abbrev-commit --stat --color=always (echo {} | cut -d" " -f1)') &&
    git checkout $(echo "$branch" | cut -d' ' -f1)
} 
# }}}1

fco() { # fuzzy checkout {{{1
  local tags branches target
  branches=$(
    git --no-pager branch --all \
      --format="%(if)%(HEAD)%(then)%(else)%(if:equals=HEAD)%(refname:strip=3)%(then)%(else)%1B[0;34;1mbranch%09%1B[m%(refname:short)%(end)%(end)" \
    | sed '/^$/d') || return
  tags=$(
    git --no-pager tag | awk '{print "\x1b[35;1mtag\x1b[m\t" $1}') || return
  target=$(
    (echo "$branches"; echo "$tags") |
    fzf --no-hscroll --no-multi -n 2 \
        --ansi) || return
  git checkout $(awk '{print $2}' <<<"$target" )
}
# }}}1

fgst() { # fuzzy pick from git status -s {{{1
  is_in_git_repo || return 1

  local cmd="${FZF_CTRL_T_COMMAND:-"command git status -s"}"

  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" fzf -m "$@" | while read -r item; do
    echo "$item" | awk '{print $2}'
  done
  echo
}
# }}}1

case "$1" in
    "show") fshow ;;
    "co" | "checkout") fco ;;
    "s" | "stat" | "status") fgst ;;
    *) echo "$USAGE" ;;
esac


