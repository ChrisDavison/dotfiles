; ^ CTRL
; # WIN
; ! ALT
; + SHIFT
SetCapsLockState, alwaysoff
; WindowManip provides:
; Resize, Move, Center, and GetCurrentMonitor
#Include, WindowManip.ahk

; Media functionality
CapsLock::F13

CapsLock & w::SoundSet, +10
CapsLock & s::SoundSet, -10
CapsLock & a::Send {Media_Prev}
CapsLock & d::Send {Media_Next}
CapsLock & Space::Send {Media_Play_Pause}

; UTILITY
^#!SPACE::WinSet, Alwaysontop, , A
^F12::reload

; Move and resize windows
; (usually for shoving youtube into a bottom corner)
^#!Left::
    Resize(480, 320)
    Move("left", "bottom")
return
^#!Right::
    Resize(480, 320)
    Move("right", "bottom")
return

#s::switchDesktop()

switchedDesktop := false
switchDesktop()
{
  global switchedDesktop
    if switchedDesktop
    {
        SendEvent ^#{Right}
        switchedDesktop := false
    }
    else
    {
        SendEvent ^#{Left}
        switchedDesktop := true
    }
}

^+1::Run Bookmarks.ahk
