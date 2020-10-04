#!/bin/bash
# browser=google-chrome
browser=firefox

songinfo=`playerctl metadata --format "{{ title }} {{ artist }}"`
searchUrl="https://search.azlyrics.com/search.php?q=$songinfo"

$browser --new-tab "$searchUrl"

