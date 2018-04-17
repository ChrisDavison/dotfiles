export LOGBOOK_DIR="$HOME/src/github.com/chrisdavison/logbook/"

this_weeks_logbook(){
    a=$(date +"%Y/")
    a+=$(date -v -monday +"%Y%m%d")
    a+="--"
    a+=$(date -v +sunday +"%Y%m%d")
    a+=".md"
    echo $LOGBOOK_DIR$a
}

last_weeks_logbook(){
    a=$(date +"%Y/")
    a+=$(date -v -monday -v-7d +"%Y%m%d")
    a+="--"
    a+=$(date -v +sunday -v-7d +"%Y%m%d")
    a+=".md"
    echo $LOGBOOK_DIR$a
}
