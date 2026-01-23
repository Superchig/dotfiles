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

# https://gist.github.com/progzone122/0b4e2a85ea44d0dc1e74fc16ee4d9700
yay --needed -S dropbox zsh-theme-powerlevel10k-git epson-inkjet-printer-escpr

# Enable git integration with gnome-keyring
# Remember to modify /etc/pam.d/login and /etc/pam.d/passwd based off of
# https://wiki.archlinux.org/index.php/GNOME/Keyring
git config --global credential.helper /usr/lib/git-core/git-credential-libsecret

# Return to original directory
cd "$ORIGDIR"

echo "Setting zsh as login shell"

chsh -s /usr/bin/zsh

echo "Checking that hostname is set"

cat /etc/hostname

echo "Setting time zone"

sudo timedatectl set-timezone America/Los_Angeles

echo "Enabling ufw"

sudo ufw enable
sudo systemctl enable ufw.service # Make it available on boot

echo "Checking that the Gnome Keyring is automatically unlocked"

if ! grep -q "auth       optional     pam_gnome_keyring.so" /etc/pam.d/login; then
  echo "auth       optional     pam_gnome_keyring.so" | sudo tee -a /etc/pam.d/login >/dev/null
fi
if ! grep -q "session    optional     pam_gnome_keyring.so auto_start" /etc/pam.d/login; then
  echo "session    optional     pam_gnome_keyring.so auto_start" | sudo tee -a /etc/pam.d/login >/dev/null
fi

echo "Enabling gcr-ssh-agent.socket to use Gnome Keyring for ssh-agent"

systemctl enable --user --now gcr-ssh-agent.socket

echo "Enabling printers with CUPS"

sudo systemctl enable --now cups.service
sudo systemctl enable --now avahi-daemon.service

echo "Enabling periodic fstrim for the SSD"

sudo systemctl enable --now fstrim.timer

echo "Stowing dotfiles"

./stow_dotfiles

echo "Symlinking custom fonts directory"

ln -s "$HOME/Dropbox/fonts" "$HOME/.local/state/fonts"
