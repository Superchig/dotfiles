#!/usr/bin/env bash
# Install programs on pacman-based distros (Arch, Antergos, Manjaro, etc.)
set -euo pipefail
IFS=$'\n\t'

ORIGDIR=$(pwd)

sudo pacman --needed -S fish xorg-server xfce4 neovim xsel base-devel vim git \
	firefox xorg-xrandr arandr sxiv guake gcc clang fd exa xterm xfce4-notifyd \
	i3-gaps rofi autorandr feh picom redshift alacritty mpv wget curl \
	xss-lock vlc pulseaudio pulseaudio-alsa pavucontrol perl-file-mimeinfo \
	discord shellcheck gnome-keyring seahorse qalculate-gtk \
	adobe-source-han-sans-otc-fonts adobe-source-han-serif-otc-fonts \
	network-manager-applet zathura zathura-pdf-poppler htop code fzf nnn \
	ttf-ubuntu-font-family kitty kitty-terminfo highlight mediainfo mlocate \
	noto-fonts-emoji ttf-joypixels xdotool wmctrl

# Install yay
if [ ! -d "$HOME"/Downloads ]; then
	mkdir "$HOME"/Downloads
fi

cd "$HOME"/Downloads
git clone https://github.com/Jguer/yay
cd yay
makepkg -si

yay -S dropbox brave-bin xcwd-git lf-bin polybar-git

# Enable git integration with gnome-keyring
# Remember to modify /etc/pam.d/login and /etc/pam.d/passwd based off of
# https://wiki.archlinux.org/index.php/GNOME/Keyring
git config --global credential.helper /usr/lib/git-core/git-credential-libsecret

# Return to original directory
cd "$ORIGDIR"
