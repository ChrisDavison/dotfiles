#!/bin/bash

menu="EVERYDAY
Weather
Email
Calendar
Youtube
9gag
ASMR"

if [ -z "$@" ]; then
    cat <<< $menu  
else
    option=$1
    if [ ! -z "$option" ]; then
        case $(echo "$option" | cut -d' ' -f1) in
            EVERYDAY) firefox --new-tab "https://news.ycombinator.com" --new-tab "https://lobste.rs" --new-tab "https://reddit.com/r/programming"  --new-tab "https://reddit.com/r/rust" --new-tab "https://reddit.com/r/videos" --new-tab "https://www.bbc.co.uk/news" ;;
            Weather) firefox --new-tab "https://darksky.net/forecast/55.8259,-4.2265/uk212/en" ;;
            Email) firefox --new-tab "https://outlook.office365.com/mail/inbox" --new-tab "https://mail.google.com" ;;
            Calendar) firefox --new-tab "https://outlook.office365.com/calendar/view/workweek" --new-tab "https://calendar.google.com" ;;
            Youtube) firefox --new-tab "https://www.youtube.com/playlist?list=WL" --new-tab "https://www.youtube.com/feed/subscriptions" --new-tab "https://www.youtube.com/";;
            9gag) firefox --new-tab "https://www.9gag.com" --new-tab "https://www.9gag.com/girl" --new-tab "https://www.9gag.com/nsfw";;
            ASMR) firefox --new-tab "$(randomasmr)" ;;
            *) exit 1 ;;
        esac
    fi
fi

