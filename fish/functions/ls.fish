# Defined in - @ line 0
function ls --description 'alias ls exa --group-directories-first --git-ignore'
	exa --group-directories-first --git-ignore $argv;
end
