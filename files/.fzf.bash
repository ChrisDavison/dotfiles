# Setup fzf
# ---------
if [[ ! "$PATH" =~ "/usr/local/Cellar/fzf/HEAD/bin" ]]; then
  export PATH="$PATH:/usr/local/Cellar/fzf/HEAD/bin"
fi

# Man path
# --------
if [[ ! "$MANPATH" =~ "/usr/local/Cellar/fzf/HEAD/man" && -d "/usr/local/Cellar/fzf/HEAD/man" ]]; then
  export MANPATH="$MANPATH:/usr/local/Cellar/fzf/HEAD/man"
fi

# Auto-completion
# ---------------
[[ $- =~ i ]] && source "/usr/local/Cellar/fzf/HEAD/shell/completion.bash"

# Key bindings
# ------------
source "/usr/local/Cellar/fzf/HEAD/shell/key-bindings.bash"

