; ^ CTRL
; # WIN
; ! ALT
; + SHIFT
SetCapsLockState, alwaysoff

; DON'T DECLARE KEYBINDS IN ANY OTHER FILES.
; Those are only for functions, so all keybinds are declared in here.

; WindowManip provides:
; Resize, Move, Center, and GetCurrentMonitor
#Include, WindowManip.ahk

; Text substitution.  Dates, and personal pinboard
#Include, TextSub.ahk

; Media functionality
+^PgDn::Send  {Volume_Down}
+^PgUp::Send {Volume_Up}
+^Del::Send {Media_Prev}
+^End::Send {Media_Next}
+^Home::Send  {Media_Play_Pause}
CapsLock::F13

; UTILITY
^#!SPACE::WinSet, Alwaysontop, , A
^F12::reload
^#!o::   ; Open my 'default' tabs and apps
    run "https://www.pinboard.in/u:davison"
    run "https://www.pinboard.com/u:davison/untagged"
    run "https://www.todoist.com"
    run "https://www.youtube.com"
    run "https://www.feedly.com"
    run "https://mail.google.com"
return

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
