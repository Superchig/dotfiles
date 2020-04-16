#!/usr/bin/env sh

OPTIONS="Quit|Don't"
CHOICE=$( echo "$OPTIONS" | rofi -dmenu -sep "|" -p "Quit bspwm?" --normal-window )

case $CHOICE in
	"Quit")
		echo "Quitting from menu..."
		bspc quit
esac
