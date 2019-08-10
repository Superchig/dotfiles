#!/usr/bin/env bash
# Install programs on pacman-based distros (Arch, Antergos, Manjaro, etc.)
set -euo pipefail
IFS=$'\n\t'

AUR_HELPER=yay

sudo pacman -S zsh emacs guake chromium vlc mpv gimp htop scrot aria2 \
	 jre7-openjdk jdk7-openjdk jre8-openjdk jdk8-openjdk \
	 conky mpd ncmpcpp adobe-source-han-sans-otc-fonts otf-ipafont

# Make sure to enable the following in /etc/locale.gen
# ja_JP.UTF-8 zh_CN.UTF-8

# $AUR_HELPER -S neofetch ghetto-skype neomutt urlview turtl encryptr

./distro-agnostic-scripts-install.sh
