#!/bin/nu

# vim: ft=sh
# vim: tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab

# https://www.nushell.sh/blog/2021-07-13-nushell_0_34.html#improved-multiline-support-jt
sudo pacman --needed -S [
  xorg-server,
  # Basic system packages
  base-devel, dkms, linux-headers, pacman-contrib
  ## Needed to run appimages
  fuse
  # Fonts
  ttf-ubuntu-font-family
  # Terminal-related
  ## Terminals
  alacritty, xterm, kitty, kitty-terminfo
  ## Foundational terminal packages
  vim, vi, git, neovim, xsel, gcc, clang, wget, curl, perl-file-mimeinfo
  mlocate, inetutils, tree
  ## Terminal utilities
  fd, exa, visidata, htop, btop, lazygit, stow, bat, yt-dlp
  neofetch, shellcheck, fzf, highlight, mediainfo, jq
  ## Shell-boosters
  fish, zsh, mcfly, zoxide
  zsh-syntax-highlighting, zsh-autosuggestions, zsh-history-substring-search
  # Desktop Suite
  ## Packages For Any Desktop
  pipewire-pulse, pipewire-alsa, gnome-keyring, gnome-tweaks, gnome-clocks
  cups, cups-pdf
  ## Gnome Desktop
  gnome-shell, gnome-control-center, gnome-backgrounds
  ## Mega-Customized Desktop (Xorg)
  i3-gaps, autorandr, xdotool, wmctrl, xorg-xrandr, rofi, picom, redshift
  xss-lock, network-manager-applet, xorg-xinput, arandr, feh, xfce4-notifyd
  ## Mega-Customized Desktop (Sway)
  sway, waybar, mako, wlroots, wl-clipboard, kanshi, egl-wayland, bemenu-wyland
  slurp, grim
  ## Tools For Any Desktop
  firefox, sxiv, mpv, vlc, pavucontrol, discord, seahorse, qalculate-gtk
  zathura, zathura-pdf-poppler, code, audacity, gimp
  ## Fonts
  adobe-source-han-sans-otc-fonts, adobe-source-han-serif-otc-fonts
  noto-fonts-emoji, ttf-joypixels, ttf-inconsolata
]
