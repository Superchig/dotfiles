#!/usr/bin/env sh
# Choose between hibernate/suspend/poweroff options via rofi

OPTIONS="Hibernate|Suspend|Power off|Restart"
CHOICE=$( echo "$OPTIONS" | rofi -dmenu -sep "|" -p "Options" --normal-window )

case $CHOICE in
  "Hibernate")
    echo "Hibernating..."
    systemctl hibernate
    ;;
  "Suspend")
    echo "Suspending..."
    systemctl suspend
    ;;
  "Power off")
    echo "Powering off..."
    systemctl poweroff
    ;;
  "Restart")
    echo "Restarting..."
    systemctl restart
    ;;
  *) echo "That's not a valid choice!";;
esac
