# Use cat or less, depending on linecount
catless() {
    height=$(tput lines)
    fileheight=$(wc -l $1 | awk '{print $1}')
    if [[ $height -gt $fileheight ]]
    then
        cat $1
    else
        less $1
    fi
}

mans () {
    man $1 | grep -iC2 --color=always $2 | less
}

remspace () {
    rename 's/\s+/_/g' "$1"
}

extractFilenameNoExt () {
    filename=$(basename "$1")
    file_no_ext="${filename%.*}"
    echo $file_no_ext
}

running() {
    ps | tr -s " " | cut -d' ' -f 3- | awk 'NR>1{print}'
}

tm() {
    local session
    newsession=${1:-new}
    session=$(tmux list-sessions -F "#{session_name}" | \
        fzf --query="$1" --select-1 --exit-0) &&
        tmux attach-session -t "$session" || tmux new-session -s $newsession
}

mostCommonWords() {
    cat $1 | tr -c '[:alnum:]' '[\n*]' | sort | uniq -c | sort -nr | head -$2
}

MoveNamedScreenshot() {
    if ! [ -d $2 ]; then
        echo "Usage: MoveNamedScreenshot <FILE> <TARGET DIR> <NEW NAME>"
        return -2
    fi
    if ! [ -f $1 ]; then
      echo "Usage: MoveNamedScreenshot <FILE> <TARGET DIR> <NEW NAME>"
      return -1
    fi
    local fn
    fn=$2"/"$3
    echo "../"$(basename $2)"/"$3 | pbcopy
    mv $1 $fn
}

literatureTitle() {
    authors="$(slugify "$1")"
    year=$2
    title="$(slugify "$3")"
    ext=$4
    echo "$year--$authors--$title.$ext"
}

uuidrecord() {
    id="$(uuid)"
    echo $id > ~/.lastuuid
    cat ~/.lastuuid
}

uuidlast() {
    [ -f ~/.lastuuid ] && cat ~/.lastuuid
}

ppath() {
  echo $PATH | tr ":" "\n" | \
    awk "{ sub(\"/usr\",   \"$fg_no_bold[green]/usr$reset_color\"); \
           sub(\"/bin\",   \"$fg_no_bold[blue]/bin$reset_color\"); \
           sub(\"/opt\",   \"$fg_no_bold[cyan]/opt$reset_color\"); \
           sub(\"/sbin\",  \"$fg_no_bold[magenta]/sbin$reset_color\"); \
           sub(\"/local\", \"$fg_no_bold[yellow]/local$reset_color\"); \
           print }"
}

myip() {
  ifconfig lo0 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "lo0       : " $2}'
  ifconfig en0 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "en0 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
  ifconfig en0 | grep 'inet6 ' | sed -e 's/ / /' | awk '{print "en0 (IPv6): " $2 " " $3 " " $4 " " $5 " " $6}'
  ifconfig en1 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "en1 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
  ifconfig en1 | grep 'inet6 ' | sed -e 's/ / /' | awk '{print "en1 (IPv6): " $2 " " $3 " " $4 " " $5 " " $6}'
}
