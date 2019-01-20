; TEXT SUBSTITUTION
::]pbd::https://pinboard.in/u:davison/<LEFT>
::]dd::   ; Insert 2019-01-01 16:40 (e.g. current date/time)
    FormatTime, Now,, yyyy-MM-dd HH:mm
    SendInput %Now%
return
::]tt::   ; Insert 16:40 (e.g. current time)
    FormatTime, Now,, HH:mm
    SendInput %Now%
return
