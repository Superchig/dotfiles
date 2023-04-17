DetectHiddenWindows(true)
SetTitleMatchMode('RegEx')

ahkHwnd := WinExist('C:\\Users\\.*\\AppData\\Roaming\\Microsoft\\Windows\\Start Menu\\Programs\\Startup\\alt_backtick.exe.*')
WinClose(ahkHwnd)