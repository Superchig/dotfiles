# vim: sw=2

Set-Alias -Name l -Value ls

Set-Alias -Name e -Value nvim

# Function must be named according to <ApprovedVerb>-<Prefix><SingularNoun>
# https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/09-functions?view=powershell-7.1
function Edit-Profile {nvim.exe $profile}
Set-Alias -Name ep -Value Edit-Profile

# Use Emacs key bindings, as per PSReadLine
# https://docs.microsoft.com/en-us/powershell/module/psreadline/about/about_psreadline?view=powershell-7.1
Set-PSReadLineOption -EditMode Emacs

Set-PSReadLineOption -PredictionSource History

if (Get-Module -ListAvailable -Name Get-ChildItemColor) {
  Import-Module Get-ChildItemColor
} else {
  echo You can install Get-ChildItemColor for colorized ls output.
  # sudo Install-Module -AllowClobber Get-ChildItemColor
}

function Move-Lfcd {
  lf.exe -last-dir-path $env:TEMP\lf\last_dir  
  Set-Location $(Get-Content $env:TEMP\lf\last_dir)
}
Set-PSReadLineKeyHandler -Chord Ctrl+o -ScriptBlock {
    [Microsoft.PowerShell.PSConsoleReadLine]::RevertLine()
    [Microsoft.PowerShell.PSConsoleReadLine]::Insert('Move-Lfcd')
    [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine()
}

function New-Link ($target, $link) {
    New-Item -Path $link -ItemType SymbolicLink -Value $target
}
Set-Alias -Name ln -Value New-Link

# gsudo is a sudo equivalent for Windows
# https://github.com/gerardog/gsudo

function Run-Lazygit {
  lazygit
}
Set-Alias -Name lg -Value Run-Lazygit
