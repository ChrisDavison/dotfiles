now=$(date +%s)
while read line; do
    scheduleFromLine=$(echo "$line" | egrep -o 't:20[0-9]{2}-[0-9]{2}-[0-9]{2}')
    dateFromLine=$(echo "$scheduleFromLine" | cut -c3-)
    if [ "$dateFromLine" = "" ]; then
        echo "$line"
    else
        dateAsEpochSeconds=$(date -d "$dateFromLine" +%s)
        if [ "$now" -gt "$dateAsEpochSeconds" ]; then
            echo "$line"
        fi
    fi
done
