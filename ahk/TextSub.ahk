; TEXT SUBSTITUTION
::;dtlong::   ; Insert 2019-01-01 16:40 (e.g. current date/time)
    FormatTime, Now,, yyyy-MM-dd HH:mm
    SendInput %Now%
return
::;dthead::   ; Insert 2019-01-01 - Mon (e.g. logbook head)
    FormatTime, Now,, yyyy-MM-dd - ddd
    SendInput %Now%
return
::;dttime::   ; Insert 16:40 (e.g. current time)
    FormatTime, Now,, HH:mm
    SendInput %Now%
return
::;dtdate::   ; Insert 2019-01 (e.g. current date)
    FormatTime, Now,, yyyy-mm
    SendInput %Now%
return
::@@h::c.jr.davison@gmail.com
::@@w::christopher.davison@strath.ac.uk
::@@cm::cmichie@strath.ac.uk
::@@ia::iandonovic@strath.ac.uk
::@@ct::ctachtatzis@strath.ac.uk
