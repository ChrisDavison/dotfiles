alias dpandoc="docker run -it --rm -v `pwd`:/source jagregory/pandoc"
alias dipy="docker run -it --rm -v `pwd`:/home/jovyan/work/ jupyter/tensorflow-notebook start.sh ipython"

function dkotlin (){
    inp="$1"; shift;
    out=$(extractFilenameNoExt $inp).jar
    docker run -it --rm -v `pwd`:/home/ zenika/alpine-kotlin kotlinc /home/"$inp" -include-runtime -d /home/"$out"
}
