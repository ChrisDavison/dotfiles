[ -f "$HOME/.zshrc" ] && rm "$HOME/.zshrc" || echo "Error removing .zshrc"
[ -f "$HOME/.vimrc" ] && rm "$HOME/.vimrc" || echo "Error removing .vimrc"
[ -f "$HOME/.sqliterc" ] && rm "$HOME/.sqliterc" || echo "Error removing .sqliterc"
[ -f "$HOME/.tmux.conf" ] && rm "$HOME/.tmux.conf" || echo "Error removing .tmux.conf"
[ -f "$HOME/.gitconfig" ] && rm "$HOME/.gitconfig" || echo "Error removing .gitconfig"

[ -d ~/.vim ] && rm -rf ~/.vim || echo "Error removing ~/.vim"
