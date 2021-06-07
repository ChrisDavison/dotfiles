# WSL2 - Launch Emacs

# Get the IP to use from wsl and set to a variable.
$wslip = wsl -d Ubuntu-20.04 bash -c 'ip route | awk ''/default via /'' | cut -d'' '' -f3'

# Run Emacs
# wsl -d Ubuntu-21.04 bash -c "export DISPLAY=$wslip`:0.0 export LIBGL_ALWAYS_INDIRECT=1 && setsid emacs"
$x410 = Get-Process x410 -ea SilentlyContinue
if ($x410) {
    Stop-Process $x410
}
sleep 2
x410.exe

# Use this to also set the keyboard layout to US
wsl -d Ubuntu-20.04 bash -c "export DISPLAY=$wslip`:0.0 export LIBGL_ALWAYS_INDIRECT=1 && setxkbmap -layout us && source /home/davison/code/dotfiles/wsl.sh && setsid emacs"
