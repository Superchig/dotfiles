#!/usr/bin/env python

# vim: tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab

import subprocess

packages = [
  "xorg-server",
  # Basic system packages
  "base-devel, dkms, linux-headers, pacman-contrib",
  ## Needed to run appimages
  "fuse",
  # Fonts
  "ttf-ubuntu-font-family",
  # Terminal-related
  ## Terminals
  "alacritty, xterm, kitty, kitty-terminfo",
  ## Foundational terminal packages
  "vim, vi, git, neovim, xsel, gcc, clang, wget, curl, perl-file-mimeinfo",
  "mlocate, inetutils, tree",
  ## Terminal utilities
  "fd, exa, visidata, htop, btop, lazygit, stow, bat, yt-dlp",
  "shellcheck, fzf, highlight, mediainfo, jq, dua-cli,",
  "tokei",
  ## Shell-boosters
  "fish, zsh, mcfly, zoxide",
  "zsh-syntax-highlighting, zsh-autosuggestions, zsh-history-substring-search",
  # Desktop Suite
  ## Packages For Any Desktop
  "pipewire-pulse, pipewire-alsa, gnome-keyring, gnome-tweaks, gnome-clocks",
  "cups, cups-pdf",
  ## Gnome Desktop
  "gnome-shell, gnome-control-center, gnome-backgrounds",
  ## Mega-Customized Desktop (Xorg)
  "i3-gaps, autorandr, xdotool, wmctrl, xorg-xrandr, rofi, picom, redshift",
  "xss-lock, network-manager-applet, xorg-xinput, arandr, feh, xfce4-notifyd",
  ## Mega-Customized Desktop (Sway)
  "sway, swaybg, waybar, mako, wl-clipboard, kanshi, egl-wayland, bemenu-wayland",
  "slurp, grim",
  ## Tools For Any Desktop
  "firefox, sxiv, mpv, vlc, pavucontrol, discord, seahorse, qalculate-gtk",
  "zathura, zathura-pdf-poppler, code, audacity, gimp",
  ## Fonts
  "adobe-source-han-sans-otc-fonts, adobe-source-han-serif-otc-fonts",
  "noto-fonts-emoji, ttf-inconsolata",
]

individual_packages = []
for package_list in packages:
    for package in package_list.split(" "):
        package = package.rstrip(",")
        # print(package)
        individual_packages.append(package)

cmd = ["sudo", "pacman", "--needed", "-S"] + individual_packages

print(" ".join(cmd))

subprocess.run(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
