#!/usr/bin/env sh
# Requires xdotool

# Used like a crufty wmctrl, since I can't seem to get wmctrl to work normally
movetodiscord() {
	xdotool search --class Discord | while read WID; do
		if ! xdotool windowactivate $WID; then
			break
		fi
	done
}

if ps -e | grep Discord && wmctrl -l | grep Discord; then
	echo "Moving to Discord..."

	movetodiscord
# wmctrl -a Discord
elif ps -e | grep Discord; then
	discord

	movetodiscord
else
	exec discord
fi
