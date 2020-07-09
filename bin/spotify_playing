#!/usr/bin/env bash

title=`playerctl metadata --format "{{ title }}, by {{ artist }} ({{ album }})"`
if [ -z "$title" ] || [ "$(playerctl status)" = 'Stopped' ] ; then
    echo "No music playing"
else
    status=`playerctl status`
    echo "$status: $title"
fi
