function tmc
set -l chosen (tmux ls | cut -d: -f1 | fzf)
if not test -z $chosen
tmux attach -t $chosen
end
end
