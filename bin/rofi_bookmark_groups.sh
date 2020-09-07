#!/bin/bash

menu="EVERYDAY
Weather
Email & Calendar
Youtube
ASMR"

if [ -z "$@" ]; then
    cat <<< $menu  
else
    option=$1
    if [ ! -z "$option" ]; then
        case $(echo "$option" | cut -d' ' -f1) in
            EVERYDAY) firefox "https://news.ycombinator.com" "https://lobste.rs" "https://reddit.com/r/programming"  "https://reddit.com/r/rust" "https://reddit.com/r/videos" "https://www.bbc.co.uk/news" ;;
            Weather) firefox "https://darksky.net/forecast/55.8259,-4.2265/uk212/en" "https://www.bbc.co.uk/weather/2648579";;
            Email) firefox "https://outlook.office365.com/mail/inbox" "https://outlook.office365.com/calendar/item/AAMkAGQ3ODUwN2Y5LTg5YzEtNDBkYi1iYjVkLTQyZDgxNWZlZDkyOQFRAAgI2FLA9sGAAEYAAAAA6FsmJnOPfkKEN5850J0uWAcA%2BMUrbkCZYUKeIrMyzWO8EAAAAJ%2B%2B%2BwAAA1s64XCi4ku4KVTw9pXnrwAAtjxYwgAAEA%3D%3D" "https://mail.google.com" "https://calendar.google.com" ;;
            Youtube) firefox "https://www.youtube.com/playlist?list=WL" "https://www.youtube.com/feed/subscriptions" "https://www.youtube.com/";;
            ASMR) firefox "$(randomasmr)" ;;
            *) exit 1 ;;
        esac
    fi
fi

