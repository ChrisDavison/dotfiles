#!/usr/bin/env fish

function tt
    echo $argv
    if test (count $argv) -lt 3
        echo "usage: tt <tunnel-server> <server> <port>"
        return
    end
    set mid $argv[1]
    set target $argv[2]
    set port $argv[3]
    echo "ssh to "$target" via "$mid" with port "$port
    ssh -t -L 8888:localhost:$port cdavison@$mid ssh -L $port:localhost:$port cdavison@$target
end
