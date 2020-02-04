#!/usr/bin/env bash
# Requires xsel and rofi

# Function from
# https://stackoverflow.com/questions/1527049/how-can-i-join-elements-of-an-array-in-bash
function join_by { local IFS="$1"; shift; echo "$*"; }

join_by ' ' $(rofi -dmenu -p "insert text" | grep -o .) | tr -d '\n' | xsel -ib
