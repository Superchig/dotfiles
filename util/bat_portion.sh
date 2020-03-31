#!/usr/bin/env sh
# Requires bc

CHARGE_FULL=$(cat /sys/class/power_supply/BAT?/charge_full | head -n 1)
CHARGE_FULL_DESIGN=$(cat /sys/class/power_supply/BAT?/charge_full_design | head -n 1)

echo "scale=4; $CHARGE_FULL / $CHARGE_FULL_DESIGN" | bc
