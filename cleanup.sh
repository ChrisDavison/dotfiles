removeOrError(){
    target=~/"$1"
    if [ -f ${target} ]; then
        rm "${target}" || echo "Error removing ${target}"
    else
        echo "${target} doesn't exist"
    fi
}
removeOrError ".zshenv"
removeOrError ".vimrc"
removeOrError ".sqliterc"
removeOrError ".tmux.conf"
removeOrError ".gitconfig"

if [ -d ~/.vim ]; then
    rm -rf ~/.vim || echo "Error removing vim"
else
    echo "~/.vim doesn't exist"
fi
