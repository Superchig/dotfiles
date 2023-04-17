#Requires AutoHotkey v2.0-a

; vim: sw=4

!`:: {
    activeHwnd := WinActive("A")
    oldClass := WinGetClass(activeHwnd)
    activeProcessName := WinGetProcessName(activeHwnd)
    winClassCount := WinGetCount("ahk_exe " . activeProcessName)

    if (winClassCount = 1)
        return

    Loop 2 {
        ; TODO(Chris): Move next window to the top, rather than current window to the bottom
        WinMoveBottom({ Hwnd: activeHwnd })
        WinActivate("ahk_exe " . activeProcessName)
        NewClass := WinGetClass(WinActive("A"))

        ; TODO(Chris): Figure out bug with explorer window
        if (oldClass != "CabinetWClass" || NewClass = "CabinetWClass")
            break
    }
}