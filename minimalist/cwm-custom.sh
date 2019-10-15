#!/usr/bin/env sh
# Startup script for cwm, used mainly to autostart programs

bash "$HOME"/dotfiles/minimalist/cwm-poststartup.bash &

exec cwm
