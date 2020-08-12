ni(){ # {{{1
    notename="$HOME/code/knowledge/inbox.md"
    echo "- $@" >> $notename
    mdformatwrap $notename
} # }}}1

niv(){ # {{{1
    bat "$HOME/code/knowledge/inbox.md"
} # }}}1

note(){ # {{{1
    notename="$HOME/code/knowledge/inbox.md"
    nvim $notename
    mdformatwrap $notename
    clear
    ls
} # }}}1

notes(){ # {{{1
    pushd $HOME/code/knowledge/
    clear
    ls
} # }}}1


