#!/bin/nu

# vim: ft=sh
# vim: tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab

# https://www.nushell.sh/blog/2021-07-13-nushell_0_34.html#improved-multiline-support-jt
sudo pacman --needed -S [
  xorg-server,
  # Basic system packages
  base-devel, dkms, linux-headers
  # Fonts
  ttf-ubuntu-font-family
  # Terminal-related
  ## Terminals
  alacritty, xterm, kitty, kitty-terminfo
  ## Foundational terminal packages
  vim, vi, git, neovim, xsel, gcc, clang, wget, curl, perl-file-mimeinfo
  mlocate, inetutils
  ## Terminal utilities
  fd, exa, visidata, htop, lazygit
  neofetch, shellcheck, fzf, highlight, mediainfo
  ## Shell-boosters
  fish, zsh, mcfly, zoxide
  zsh-syntax-highlighting, zsh-autosuggestions, zsh-history-substring-search
  # Desktop Suite
  ## Packages For Any Desktop
  pipewire-pulse, pipewire-alsa, gnome-keyring
  ## Gnome Desktop
  gnome-shell, gnome-control-center, gnome-backgrounds
  ## Mega-Customized Desktop (Xorg)
  i3-gaps, autorandr, xdotool, wmctrl, xorg-xrandr, rofi, picom, redshift
  xss-lock, network-manager-applet, xorg-xinput, arandr, feh, xfce4-notifyd
  ## Tools For Any Desktop
  firefox, sxiv, mpv, vlc, pavucontrol, discord, seahorse, qalculate-gtk
  zathura, zathura-pdf-poppler, code, audacity, gimp
  ## Fonts
  adobe-source-han-sans-otc-fonts, adobe-source-han-serif-otc-fonts
  noto-fonts-emoji, ttf-joypixels, ttf-inconsolata
]
