#!/usr/bin/env bash
# vim: tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
# Install programs on pacman-based distros (Arch, Antergos, Manjaro, etc.)

set -euo pipefail
IFS=$'\n\t'

ORIGDIR=$(pwd)

sudo pacman --needed -S nushell

nu pacman-install.nu

# Install paru
if ! command -v paru 2>&1; then
  if [ ! -d "$HOME"/Downloads ]; then
    mkdir "$HOME"/Downloads
  fi

  cd "$HOME"/Downloads
  git clone https://aur.archlinux.org/paru-bin.git
  cd paru
  makepkg -si
fi

paru --needed -S dropbox xcwd-git lf-bin polybar-git zsh-theme-powerlevel10k-git noisetorch-bin \
  frum-bin fnm-bin extension-manager

# Enable git integration with gnome-keyring
# Remember to modify /etc/pam.d/login and /etc/pam.d/passwd based off of
# https://wiki.archlinux.org/index.php/GNOME/Keyring
git config --global credential.helper /usr/lib/git-core/git-credential-libsecret

# Return to original directory
cd "$ORIGDIR"
