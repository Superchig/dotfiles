#!/usr/bin/env sh

OPTIONS="Quit|Don't"
CHOICE=$( echo "$OPTIONS" | rofi -dmenu -sep "|" -p "Quit i3?" --normal-window )

case $CHOICE in
	"Quit")
		echo "Quitting from menu..."
		i3-msg exit
esac
