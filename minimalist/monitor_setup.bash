#!/usr/bin/bash

# Select eDP1 or eDP2 as the built-in screen, depending on which one is
# connected
if xrandr | grep 'eDP1 connected'; then
  eDP=eDP1
else
  eDP=eDP2
fi

# If there are no command-line arguments and the HDMI1 monitor is plugged in, activate HDMI1
if [ "$#" -eq 0 ] && xrandr | grep 'HDMI1 connected'; then
  xrandr --output $eDP --mode 1366x768
  xrandr --output HDMI1 --mode 1920x1080 --left-of $eDP --primary
elif [ "$1" = "nobuiltin" ] && xrandr | grep 'HDMI1 connected'; then
  xrandr --output $eDP --off
  xrandr --output HDMI1 --mode 1920x1080
else
  xrandr --auto
fi

# Use ~/.fehbg if it exists
if [ -f ~/.fehbg ]; then
  ~/.fehbg
fi
