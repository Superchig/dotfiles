#!/usr/bin/env bash
# Install programs on pacman-based distros (Arch, Antergos, Manjaro, etc.)
set -euo pipefail
IFS=$'\n\t'

AUR_HELPER=yay

# Removed packages: mpv scrot aria2 conky mpd ncmpcpp otf-ipafont
sudo pacman -S zsh emacs guake chromium vlc mpv gimp htop scrot aria2 neovim \
	 jre7-openjdk jdk7-openjdk jre8-openjdk jdk8-openjdk \
	 adobe-source-han-sans-otc-fonts

# Make sure to enable the following in /etc/locale.gen
# ja_JP.UTF-8 zh_CN.UTF-8

# $AUR_HELPER -S neofetch ghetto-skype neomutt urlview turtl encryptr

# ./distro-agnostic-scripts-install.sh
