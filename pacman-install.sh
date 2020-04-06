#!/usr/bin/env bash
# Install programs on pacman-based distros (Arch, Antergos, Manjaro, etc.)
set -euo pipefail
IFS=$'\n\t'

ORIGDIR=$(pwd)

sudo pacman --needed -S fish xorg-server xfce4 neovim xsel base-devel vim git \
	firefox xorg-xrandr arandr sxiv guake gcc clang fd exa xterm \
	i3-gaps rofi autorandr feh picom redshift alacritty mpv wget curl \
	xss-lock vlc pulseaudio pulseaudio-alsa pavucontrol perl-file-mimeinfo \
	discord shellcheck gnome-keyring seahorse \
	adobe-source-han-sans-otc-fonts adobe-source-han-serif-otc-fonts

# Install yay
if [ ! -d "$HOME"/Downloads ]; then
	mkdir "$HOME"/Downloads
fi

cd "$HOME"/Downloads
git clone https://github.com/Jguer/yay
cd yay
makepkg -si

# Enable git integration with gnome-keyring
git config --global credential.helper /usr/lib/git-core/git-credential-libsecret

# Return to original directory
cd "$ORIGDIR"
