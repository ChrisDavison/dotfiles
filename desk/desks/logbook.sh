# Description: desk for engd logbook

cd ~/Dropbox/logbook

# Create and edit a markdown file with todays date as the name
logbook_today () {
  vim `today`.md
}

# Create a markdown file with todays date as the name
create_logbook_today () {
  (cd ~/Dropbox/logbook && touch `today`.md)
}

desk
