if [ "$#" -lt 1 ]; then
    echo "usage: ./install_repos.sh <parent_directory_for_source>"
    return
fi
target_dir="$1"

echo "Downloading personal repos"
for repo in chrisdavison-hugo learning scripts;
do
    echo "\t$repo"
    git clone --quiet git@github.com:chrisdavison/"$repo" $HOME/src/"$repo" > /dev/null
done

echo "Downloading work repos"
for repo in cattleprod collar-outlier-removal cowhealth precisionbeef ee273 cybele-sat ;
do
    echo "\t$repo"
    git clone --quiet git@github.com:cidcom/"$repo" $HOME/src/"$repo" > /dev/null
done

