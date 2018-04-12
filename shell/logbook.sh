export LOGBOOK_DIR="$HOME/src/github.com/chrisdavison/logbook/"

function this_weeks_logbook(){
    a=$(date +"%Y/")
    a+=$(date -v -monday +"%Y%m%d")
    a+="--"
    a+=$(date -v +sunday +"%Y%m%d")
    a+=".md"
    echo $LOGBOOK_DIR$a
}
