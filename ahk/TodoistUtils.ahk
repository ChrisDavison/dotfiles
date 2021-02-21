TodoistActivate(){
    Run C:\Users\davison\AppData\Local\Programs\todoist\Todoist.exe
    sleep,200
    WinActivate, ahk_exe Todoist.exe
    sleep,150
}

TodoistAdd(){
    TodoistActivate()
    Send q
}

TodoistSearch(){
    TodoistActivate()
    Send f
}
