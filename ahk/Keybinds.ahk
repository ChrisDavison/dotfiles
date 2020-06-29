; DON'T DECLARE KEYBINDS IN ANY OTHER FILES.
; All keybinds should be declared in here only, so it's easier
; to modify, and prevent overlap,
; 
; Other scripts are only for function definitions.
;###########################################################
;  +       SHIFT
;  ^ # !   CTRL WIN ALT
;###########################################################
SetCapsLockState, alwaysoff
SendMode Input

CapsLock::F13

; Ctrl+F12 to reload this hotkey script
^F12::reload

;  Text Insertion
^#!XButton1::Send c.jr.davison@gmail.com
^#!XButton2::Send christopher.davison@strath.ac.uk
^+!::Send **({!})**

; Media functionality
CapsLock & s::Send {Volume_Down 5}
CapsLock & w::Send {Volume_Up 5}
CapsLock & a::Send {Media_Prev}
CapsLock & d::Send {Media_Next}
CapsLock & Space::Send  {Media_Play_Pause}

CapsLock & t::Run https://www.todoist.com

; F9::
; WinGet, beforeSpotify, , A
; WinActivate, Spotify
; return

; F10::WinActivate ahk_id %beforeSpotify%

;###########################################################
;               EXTERNAL SCRIPTS BELOW HERE
;###########################################################
; ^+1::Run Bookmarks.ahk

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WindowManip provides:
;;; Resize, Move, Center, and GetCurrentMonitor
#Include, WindowManip.ahk

; Shove a window into a corner (e.g. for a youtube window)
^#!Left::ResizeMove(480, 320, "left", "bottom")
^#!Right::ResizeMove(480, 320, "right", "bottom")

CapsLock & c::Center()

; Basically a cleaner name for an AHK builtin
^#!SPACE::WindowOnTop()

CapsLock & 1::MoveTo("left", "left")
CapsLock & 2::MoveTo("left", "right")
CapsLock & 3::MoveTo("right", "left")
CapsLock & 4::MoveTo("right", "right")

CapsLock & F1::FullscreenOnMonitor("left")
CapsLock & F2::FullscreenOnMonitor("right")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text substitution.  Dates, and personal pinboard
; #Include, TextSub.ahk

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SwitchDesktop is
#Include, SwitchDesktop.ahk
^#!s::switchDesktop()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Include, MoveToOtherMonitor.ahk
^!q::MoveToOtherMonitor()
