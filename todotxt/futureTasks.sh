#!/usr/bin/env sh
now=$(date +%s)
while read line; do
    scheduleFromLine=$(echo "$line" | egrep -o 't:20[0-9]{2}-[0-9]{2}-[0-9]{2}')
    date=$(echo "$scheduleFromLine" | cut -c3-)
    dateAsEpochSeconds=$(date -d "$date" +%s)
    if [ "$date" = "" ] || [ "$now" -gt "$dateAsEpochSeconds" ]; then
        echo "$line"
    fi
done
