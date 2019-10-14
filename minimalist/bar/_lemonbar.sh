#!/usr/bin/env sh
# Format output to use in lemonbar

# Define the clock
Clock() {
        DATETIME=$(date "+%a %b %d, %I:%M")

        echo -n "$DATETIME"
}

# Print the clock

while true; do
        # echo "%{c}%{F#FFFF00}%{B#0000FF} $(Clock) %{F-}%{B-} %{r}%{F#FF0000}%{B#000000}⏻ "
        echo "%{c}%{F#FFFF00}%{B#000000} $(Clock) %{r}%{F#FF0000} Batt: $(cat /sys/class/power_supply/BAT?/capacity)%"
        sleep 1
done
