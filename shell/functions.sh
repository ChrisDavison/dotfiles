inpath() { # Check ifa file is in $PATH
    type "$1" >/dev/null 2>&1;
}

nonascii() { # Ripgrep for non-ascii, greek, or "£"
    rg "[^\x00-\x7F£\p{Greek}]" -o --no-heading
}

refresh_dmenu() {
    [ -f ~/.cache/dmenu_run ] && rm ~/.cache/dmenu_run && dmenu_path
}

git_aliases (){
    git config --list | rg alias | column -s '=' -t | sort
}

is_in_git_repo() { 
  git rev-parse HEAD > /dev/null 2>&1
} 

monospace-fonts(){ 
    fc-list :mono | cut -d':' -f2  | cut -d',' -f1 | sort | uniq
} 

duplicates(){ # find duplicate words in a file 
    [[ $# -eq 0 ]] && echo "usage: duplicates <file>..." && return
    grep -Eo '(\b.+) \1\b' $1 || true
} 

due() {
    nlt.py -f "due:%Y-%m-%d" $@
}

tma() {
    if [ ! -z "$TMUX" ]; then
        echo "ALREADY IN TMUX"
        return
    fi
    chosen=`tmux ls | cut -d':' -f1 | fzf -0 -1`
    if [ ! -z "$chosen" ]; then
        tmux attach -t "$chosen"
    else
        tmux
    fi
}

winhome(){
    echo "/mnt/c/Users/davison/"$1
}

last_work_week(){
    last7days.py ~/code/logbook/`date +%Y` | bat -l md
}
alias l7w="last_work_week"

last_journal_week(){
    last7days.py ~/code/knowledge/journal | bat -l md
}
alias l7j="last_journal_week"

