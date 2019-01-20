Resize(Width, Height)
{
    WinMove, A, , , , Width, Height
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
    else
        posny := MonBoxBottom-Height
    WinMove, A, , %posnx%, %posny%, Width, Height
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
    if (winX > monAreaLeft && winX < monAreaRight && winY < monAreaBottom && winY > monAreaTop)
      return A_Index
  }
  SysGet, primaryMonitor, MonitorPrimary
  return "No Monitor Found"
}
