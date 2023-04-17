let startup_dir = $'($env.APPDATA)\Microsoft\Windows\Start Menu\Programs\Startup'
let out_path = $'($startup_dir)\alt_backtick.exe'
let base_path = $'($env.ProgramFiles)\AutoHotKey\v2\AutoHotkey64.exe'

# Compile and set up the ahk file
^'C:\Program Files\AutoHotkey\Compiler\Ahk2Exe' /in alt_backtick.ahk /out $out_path /base $base_path
