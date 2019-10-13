#!/usr/bin/bash

# If there are no command-line arguments and the HDMI1 monitor is plugged in, activate HDMI1
if [ "$#" -eq 0 ] && xrandr | grep HDMI1; then
  xrandr --output eDP1 --mode 1366x768
  xrandr --output HDMI1 --mode 1920x1080 --left-of eDP1 --primary
elif [ "$1" = "nobuiltin" ] && xrandr | grep HDMI1; then
  xrandr --output eDP1 --off
  xrandr --output HDMI1 --mode 1920x1080
else
  xrandr --auto
fi
