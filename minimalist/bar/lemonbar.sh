#!/usr/bin/env sh

# Kill all existing lemonbar instances
killall -q lemonbar

sh ./_lemonbar.sh | lemonbar -p -d -f Inconsolata &
