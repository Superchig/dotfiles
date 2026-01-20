#!/usr/bin/env bash
# vim: tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
# Install programs on pacman-based distros (Arch, Antergos, Manjaro, etc.)

set -euo pipefail
IFS=$'\n\t'

ORIGDIR=$(pwd)

./pacman-install.py

# Install yay
if ! command -v yay 2>&1; then
  if [ ! -d "$HOME"/Downloads ]; then
    mkdir "$HOME"/Downloads
  fi

  cd "$HOME"/Downloads
  git clone https://aur.archlinux.org/yay-bin.git
  cd yay-bin
  makepkg -si
fi

yay --needed -S dropbox zsh-theme-powerlevel10k-git

# Enable git integration with gnome-keyring
# Remember to modify /etc/pam.d/login and /etc/pam.d/passwd based off of
# https://wiki.archlinux.org/index.php/GNOME/Keyring
git config --global credential.helper /usr/lib/git-core/git-credential-libsecret

# Return to original directory
cd "$ORIGDIR"
