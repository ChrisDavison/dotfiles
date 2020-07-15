; TEXT SUBSTITUTION
::;datetime::   ; Insert 2019-01-01 - Mon (e.g. logbook head)
    FormatTime, Now,, dd-MM-yyyy HH:MM
    SendInput %Now%
return
::;time::   ; Insert 16:40 (e.g. current time)
    FormatTime, Now,, HH:mm
    SendInput %Now%
return
::;date::   ; Insert 2019-01 (e.g. current date)
    FormatTime, Now,, yyyy-MM-dd
    SendInput %Now%
return
::@@h::c.jr.davison@gmail.com
::@@w::christopher.davison@strath.ac.uk
::@@cm::cmichie@strath.ac.uk
::@@ia::iandonovic@strath.ac.uk
::@@ct::ctachtatzis@strath.ac.uk
