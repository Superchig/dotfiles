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
        WinMoveBottom({ Hwnd: activeHwnd })
        WinActivate("ahk_exe " . activeProcessName)
        NewClass := WinGetClass(WinActive("A"))

        if (oldClass != "CabinetWClass" || NewClass = "CabinetWClass")
            break
    }
}