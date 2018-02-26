#! /usr/bin/env bash
OLD_RED='\e[1;31m'
BOLD_GREEN='\e[1;32m'
BOLD_YELLOW='\e[1;33m'
BOLD_BLUE='\e[1;34m'
BOLD_GREY='\e[1;37m'
RED='\e[0;31m'
GREEN='\e[0;32m'
YELLOW='\e[0;33m'
BLUE='\e[0;34m'
GREY='\e[0;37m'
RESET='\e[0;39m'

local host_name="%{%F{cyan}d%}"
local path_string="%{%F{yellow}%}%~"
local prompt_string="$"

# Make prompt_string red if the previous command failed.
local return_status="%(?:%{%F{blue}%}$prompt_string:%{%F{red}%}$prompt_string)"

# PROMPT="${host_name} ${path_string} ${return_status} %{$reset_color%}"
PROMPT="${path_string} ${return_status} %{%F{reset}%}"
