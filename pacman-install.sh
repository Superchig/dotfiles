#!/usr/bin/env bash
# Install programs on pacman-based distros (Arch, Antergos, Manjaro, etc.)
set -euo pipefail
IFS=$'\n\t'

AUR_HELPER=pacaur

sudo pacman -S zsh emacs guake chromium vlc mpv gimp htop scrot aria2 \
	 jre7-openjdk jdk7-openjdk jre8-openjdk jdk8-openjdk \
	 conky

$AUR_HELPER -S neofetch ghetto-skype

./distro-agnostic-scripts-install.sh
