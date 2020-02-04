#!/usr/bin/env bash
# Requires xsel and rofi

rofi -dmenu -p "insert text" | grep -o . | tr '\n' ' ' | sed 's/.$//' | xsel -ib
