#!/bin/sh
polybar-msg cmd quit >/dev/null 2>&1
polybar >/tmp/polybar.log 2>&1
