#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
echo "---" | tee -a /var/log/polybar/polybar.log
if command -v xrandr >/dev/null; then
    for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=$m polybar --reload example >>/var/log/polybar/polybar.log 2>&1 &
    done
else
    polybar --reload example >>/var/log/polybar/polybar.log 2>&1 &
fi
