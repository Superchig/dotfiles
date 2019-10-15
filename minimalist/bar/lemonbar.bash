#!/usr/bin/env bash

# Kill all existing lemonbar instances
killall -q lemonbar

bash "${BASH_SOURCE%/*}/_lemonbar.sh" | lemonbar -p -d B#000000 -f Inconsolata &
