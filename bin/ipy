#!/usr/bin/env bash
# Run an ipython docker image, with port forwarding and current dir mounted
if docker ps -a | grep -q "ipython"; then
    docker start ipython
    docker attach ipython
else
    docker run -it -p 8889:8888 -v /home/davison:/home/jovyan --name=ipython cd-ipython:latest ipython
fi

