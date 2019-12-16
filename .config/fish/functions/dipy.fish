function dipy
    docker run -it --rm -v (pwd):/home/jovyan/(basename (pwd)) -w /home/jovyan/(basename (pwd)) jupyter/tensorflow-notebook ipython --pylab --no-banner
end
