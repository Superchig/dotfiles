mman() {
  man $(man -k "$1" | fzf | awk '{print $1}')
}
