PROMPTDIR="%1~"
NAMEANDHOST="(%n@%m)"
PROMPTCHAR="»"
SERVER=""
if [ -f ~/.servername ]; then
    contents=$(cat ~/.servername)
    SERVER="${contents} » "
fi
PROMPT="%{%F{yellow}%}${SERVER}%{%F{green}%}%}${PROMPTDIR} ${PROMPTCHAR} %{%F{reset}%}%}"
