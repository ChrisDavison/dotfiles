# fco - fuzzy checkout [C-g b]
# fshow - fuzzy show [C-g c] 
# fgst - fuzzy select from git status (vfg - vim, fuzzy from status)

# fzf utility functions {{{1
# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}
# }}}1

fshow(){ # fuzzy show commit {{{1
    local commit commits
    commits=$(git log --oneline) &&
        commit=$(echo "$commits" | fzf --preview 'git show --abbrev-commit --stat --color=always $(echo {} | cut -d" " -f1)') &&
    git checkout $(echo "$branch" | cut -d' ' -f1)
} 
zle -N fshow
bindkey '^gc' fshow
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
zle -N fco
bindkey '^gb' fco
# }}}1

fgst() { # fuzzy pick from git status -s {{{1
  # "Nothing to see here, move along"
  is_in_git_repo || return 1

  local cmd="${FZF_CTRL_T_COMMAND:-"command git status -s"}"

  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" fzf -m "$@" | while read -r item; do
    echo "$item" | awk '{print $2}'
  done
  echo
}
alias vfg='nvim $(fgst)'
# }}}1
