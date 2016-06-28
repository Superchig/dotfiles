#!/usr/bin/env bash
# Install programs on pacman-based distros (Arch, Antergos, Manjaro, etc.)
set -euo pipefail
IFS=$'\n\t'

AUR_HELPER=pacaur

sudo pacman -S zsh emacs guake chromium vlc

./distro-agnostic-scripts-install.sh
