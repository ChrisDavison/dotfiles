# Change default prefix
set -g prefix 'C-a'

# Vim-like plugins for moving and resizing panes
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

bind H resize-pane -L 5
bind J resize-pane -D 5
bind K resize-pane -U 5
bind L resize-pane -R 5

# Easier switching between windows
bind C-p previous-window
bind C-n next-window

# Use PREFIX |/- to split either vertically or horizontally
bind | split-window -h
bind - split-window -v

