;  +       SHIFT
;  ^ # !   CTRL WIN ALT
;..........................................................
SetCapsLockState, alwaysoff
SendMode Input

CapsLock::F13
^+F12::CapsLock

; Ctrl+F12 to reload this hotkey script
^F12::reload

; Media functionality
CapsLock & s::Send {Volume_Down 1}
CapsLock & w::Send {Volume_Up 1}
CapsLock & a::Send {Media_Prev}
CapsLock & d::Send {Media_Next}
CapsLock & Space::Send  {Media_Play_Pause}

CapsLock & p::run python.exe "C:\ahk\dashboard.py"

#Include, TodoistUtils.ahk
^!a::TodoistAdd()
^!t::TodoistActivate()
^!s::TodoistSearch()

#Include, AnkiUtils.ahk
^+a::AnkiQuickAdd()

CapsLock & n::WSLNvimJournal()
WSLNvimJournal(){
    run "C:\Users\davison\AppData\Local\wsltty\bin\mintty.exe" "--WSL=" "--configdir=C:\Users\davison\AppData\Roaming\wsltty" "-~"  "-e" "nvim" "-c" "NewJournal"
    sleep, 300
    Resize(1000, 1200)
    Center()
}

#Include, PrintScreenToAnkiImageOcclusion.ahk
#Include, CopyAndPasteAsPlaintext.ahk
#Include, NextBookmarkInManager.ahk
; CapsLock & v::nextBookmarkInManager(75)
	
; CapsLock & n::Send ^!n

CapsLock & XButton1::WinActivate, ahk_id %beforeSpotify%
CapsLock & XButton2::
WinGet, beforeSpotify, , A
Run, spotify.exe
WinActivate, ahk_exe spotify.exe
return

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
#Include, TextSub.ahk

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SwitchDesktop is
#Include, SwitchDesktop.ahk
^#!s::switchDesktop()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Include, MoveToOtherMonitor.ahk
^!q::MoveToOtherMonitor()
