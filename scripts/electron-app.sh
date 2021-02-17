#!/usr/bin/env sh
# Requires xdotool, wmctrl

# ./electron-app.sh process title/class

# Used like a crufty wmctrl, since I can't seem to get wmctrl to work normally
movetoapp() {
	# xdotool search --class "$1" | while read WID; do
	# 	if ! xdotool windowactivate $WID; then
	# 		break
	# 	fi
	# done
	wmctrl -a "$1"
}

# if ps -e | grep -E "$2|$1" && wmctrl -l | grep -E "$2|$1"; then
if wmctrl -l | grep -E "$2|$1"; then
	echo "Moving to "$2"..."

	movetoapp "$2"
elif ps -e | grep "$1"; then
	"$1" &
	disown

	movetoapp "$2"
else
	exec "$1"
fi
