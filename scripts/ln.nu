def main [
  refers_to: string,
  link_name: string,
  -s: bool
] {
  if not $s {
    echo "You must use `-s` to make a symbolic link. This only supports symbolic links."
    return
  }
  powershell -command $"New-Item -ItemType SymbolicLink -Path \"($link_name)\" -Target \"($refers_to)\" "
}
