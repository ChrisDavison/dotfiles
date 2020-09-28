#!/bin/bash
# browser=google-chrome
browser=firefox

menu="EVERYDAY
Weather
Email
Calendar
Youtube"
# Myplace Qute"
# 9gag

dmenu_config="-i -fn 'Hack-14' -sb #8620e6"

option=$(dmenu $dmenu_config -p "Bookmarks:" <<< $menu)
if [ ! -z "$option" ]; then
    case "$option" in
        EVERYDAY) $browser --new-tab "https://news.ycombinator.com" --new-tab "https://lobste.rs" --new-tab "https://reddit.com/r/programming"  --new-tab "https://reddit.com/r/rust" --new-tab "https://reddit.com/r/videos" --new-tab "https://www.bbc.com/news/uk" ;;
        Weather) $browser --new-tab "https://darksky.net/forecast/55.8259,-4.2265/uk212/en" ;;
        Email) $browser --new-tab "https://outlook.office365.com/mail/inbox" --new-tab "https://mail.google.com" ;;
        Calendar) $browser --new-tab "https://outlook.office365.com/calendar/view/workweek" --new-tab "https://calendar.google.com" ;;
        Youtube) $browser --new-tab "https://www.youtube.com/playlist?list=WL" --new-tab "https://www.youtube.com/feed/subscriptions" --new-tab "https://www.youtube.com/";;
        9gag) $browser --new-tab "https://www.9gag.com" --new-tab "https://www.9gag.com/girl" --new-tab "https://www.9gag.com/nsfw";;
        "Myplace Qute") i3-msg exec qutebrowser "https://classes.myplace.strath.ac.uk" > /dev/null ;;
        *) exit 1 ;;
    esac
fi

