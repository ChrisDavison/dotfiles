#!/bin/bash
echo -ne "i3-ipc\x0\x0\x0\x0\x4\x0\x0\x0" | 
    socat STDIO UNIX-CLIENT:`i3 --get-socketpath` | 
    tail -c +15 |
    sed -e 's/"id":/\n"id":/g' | 
    sed -ne 's/.*"name":"\([^"]\+\)".*"window":\([0-9]\+\).*/\1 \2/p' |
    rg -v "i3bar" |
    dmenu -i -l 10 -m 0 -fn "Ubuntu Mono-18" -p "Window: " |
    sed -ne 's/.* \([0-9]*\)/[id=\1] focus/p' |
    (read cmd; i3-msg "$cmd")
