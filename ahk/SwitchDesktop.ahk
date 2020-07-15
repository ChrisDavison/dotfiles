
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
