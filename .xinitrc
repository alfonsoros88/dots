#!/bin/bash

test -f "$HOME/.profile" && . "$HOME/.profile"

rofi &
polybar -r main &
feh --bg-scale ~/Pictures/wp/wp1.jpg &

exec xmonad
