WindowOnTop(){
    WinSet, Alwaysontop, , A
}

Resize(Width, Height)
{
    WinMove, A, , , , Width, Height
}

ResizeMove(Width, Height, locx, locy)
{
    Resize(Width, Height)
    Move(locx, locy)
}

FullscreenOnMonitor(monitor)
{
    activeWindow := WinActive("A")
    activeWindow := WinActive("A")
    SysGet, Mon, Monitor, 0
    SysGet, Mon2, Monitor, 1
    
    if %MonLeft% > %Mon2Left% 
    {
        ll := Mon2Left
        lt := Mon2Top
        rl := MonLeft
        rt := MonTop
        width := (Mon2Right - Mon2Left)
        height := (Mon2Bottom - Mon2Top)
    } 
    else {
        ll := MonLeft
        lt := MonTop
        rl := Mon2Left
        rt := Mon2Top
        width := (MonRight - MonLeft)
        height := (MonBottom - MonTop)
    }
    if monitor = left
    {
        left := ll
        top := lt
    }
    else {
        left := rl
        top := rt
    }
    WinMove, ahk_id %activeWindow%, , %left%, %top%, %width%, %height%
}

MoveTo(monitor, monitorside)
{
    activeWindow := WinActive("A")
    SysGet, Mon, Monitor, 0
    SysGet, Mon2, Monitor, 1

    if %MonLeft% > %Mon2Left% 
    {
        ll := Mon2Left
        lt := Mon2Top
        rl := MonLeft
        rt := MonTop
        width := (Mon2Right - Mon2Left) / 2
        height := (Mon2Bottom - Mon2Top)
    } 
    else {
        ll := MonLeft
        lt := MonTop
        rl := Mon2Left
        rt := Mon2Top
        width := (MonRight - MonLeft) / 2
        height := (MonBottom - MonTop)
    }
    if monitor = left
    {
        left := ll
        top := lt
    }
    else {
        left := rl
        top := rt
    }
    if monitorside = right 
    { 
        left := left + width 
    }
    WinMove, ahk_id %activeWindow%, , %left%, %top%, %width%, %height%
}

Move(locx, locy)
{
    mon := GetCurrentMonitor()
    SysGet, MonBox, Monitor, %mon%   ; Get coordinates & width of Monitor 2
    WinGetPos, , , Width, Height, A
    if ( locx = "left" )
        posnx := MonBoxLeft
    else
        posnx := MonBoxRight-Width
    if ( locy = "top" )
        posny := MonBoxTop
    else if ( locy = "mid" )
        posny := MonBoxBottom-(MonBoxBottom-MonBoxTop)/2
    else
        posny := MonBoxBottom-Height
    WinMove, A, , %posnx%, %posny%, Width, Height
}

MoveToTopHalfOfMonitor()
{
    ResizeHalfMonitor()
    Move("left", "top")
}

MoveToBottomHalfOfMonitor()
{
    ResizeHalfMonitor()
    Move("left", "mid")
}

ResizeHalfMonitor()
{
    mon := GetCurrentMonitor()
    SysGet, MonBox, Monitor, %mon%   ; Get coordinates & width of Monitor 2
    monWidth := MonBoxRight-MonBoxLeft
    halfHeight := (MonBoxBottom-MonBoxTop)/2

    Resize(monWidth, halfHeight)
}

Center()
{
    mon := GetCurrentMonitor()
    SysGet, MonBox, Monitor, %mon%   ; Get coordinates & width of Monitor 2
    WinGetPos, , , Width, Height, A
    posnx := MonBoxLeft + (MonBoxRight-MonBoxLeft)/2 - (Width/2)
    posny := MonBoxTop + (MonBoxBottom-MonBoxTop)/2 - (Height/2)
    WinMove, A, , %posnx%, %posny%, Width, Height
}

GetCurrentMonitor()
{
  SysGet, numberOfMonitors, MonitorCount
  WinGetPos, winX, winY, winWidth, winHeight, A
  Loop %numberOfMonitors%
  {
    SysGet, monArea, Monitor, %A_Index%
    if (winX >= monAreaLeft && winX <= monAreaRight)
      return A_Index
  }
  SysGet, primaryMonitor, MonitorPrimary
  return "No Monitor Found"
}
