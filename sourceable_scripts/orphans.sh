orphans() {
  sudo pacman -Rns $(pacman -Qtdq)
}
