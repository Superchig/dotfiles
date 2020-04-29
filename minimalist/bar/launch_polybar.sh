#!/usr/bin/env sh

killall -q polybar

PRIMARY=$(polybar --list-monitors | grep primary | cut -d ':' -f1)
for m in $(polybar --list-monitors | cut -d":" -f1); do
    export MONITOR=$m 
		export TRAY_POSITION=none

		if [ "$MONITOR" = "$PRIMARY" ]; then
			TRAY_POSITION=right
		fi

		polybar --reload bar &
done
