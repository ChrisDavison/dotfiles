; ^ CTRL
; # WIN
; ! ALT
; + SHIFT
SetCapsLockState, alwaysoff
^#!XButton1::SendInput c.jr.davison@gmail.com
^#!XButton2::SendInput christopher.davison@strath.ac.uk

; DON'T DECLARE KEYBINDS IN ANY OTHER FILES.
; Those are only for functions, so all keybinds are declared in here.

; WindowManip provides:
; Resize, Move, Center, and GetCurrentMonitor
#Include, WindowManip.ahk

; Text substitution.  Dates, and personal pinboard
; #Include, TextSub.ahk

; Media functionality
+^PgDn::Send  {Volume_Down}
+^PgUp::Send {Volume_Up}
+^Del::Send {Media_Prev}
+^End::Send {Media_Next}
+^Home::Send  {Media_Play_Pause}
CapsLock::F13

!^XButton1::Send  {Volume_Down}
!^XButton2::Send {Volume_Up}

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

^#!s::switchDesktop()

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
