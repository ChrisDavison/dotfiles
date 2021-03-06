#!/usr/bin/env fish

function ipy
    if not docker ps -a | grep -q "ipython"
        docker run -it -p 8889:8888 -v /home/davison:/home/jovyan --name=ipython cd-ipython:latest ipython
    else
        docker start ipython
        docker attach ipython
    end
end
