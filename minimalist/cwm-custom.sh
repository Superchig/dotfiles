#!/usr/bin/env sh
# Startup script for cwm, used mainly to autostart programs

bash /home/chiggie/dotfiles/minimalist/poststartup &

exec cwm
