#/bin/bash
dmenu_config="-i"
if [ -f "$HOME/.config/dmenu.conf" ]; then
    dmenu_config=`cat $HOME/.config/dmenu.conf`
fi
echo -ne "i3-ipc\x0\x0\x0\x0\x4\x0\x0\x0" | 
    socat STDIO UNIX-CLIENT:`i3 --get-socketpath` | 
    tail -c +15 |
    sed -e 's/"id":/\n"id":/g' | 
    sed -ne 's/.*"name":"\([^"]\+\)".*"window":\([0-9]\+\).*/\1 \2/p' |
    rg -v "i3bar" |
    dmenu -l 10 -p "Window: " $dmenu_config |
    sed -ne 's/.* \([0-9]*\)/[id=\1] focus/p' |
    (read cmd; i3-msg "$cmd")
