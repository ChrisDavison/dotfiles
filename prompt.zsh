# Only the last segment of the path as PROMPTDIR
PROMPTDIR="%1~ "
# PROMPTDIR=""
NAMEANDHOST="(%n@%m)"
PROMPTCHAR="• "
SERVER=""
if [ -f ~/.servername ]; then
    contents=$(cat ~/.servername)
    SERVER="${contents}"
fi
# PROMPT="%{%F{yellow}%}${SERVER} %{%F{green}%}%}${PROMPTDIR}${PROMPTCHAR}%{%F{reset}%}%}"
PROMPT="%{%F{green}%}%}${PROMPTDIR}${PROMPTCHAR}%{%F{reset}%}%}"
