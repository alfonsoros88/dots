#!/usr/bin/env bash
xrdb -merge ~/.Xresources

PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

if $(xrandr | awk '/DP-1-2-1 connected/'); then
    ~/.screenlayout/aid_desk.sh
fi

polybar -r bar &
feh --bg-scale ~/Pictures/wallpapers/ds_static.jpg

exec xmonad
