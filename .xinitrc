#!/usr/bin/env bash
xrdb -merge ~/.Xresources

PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

if $(xrandr | awk '/HDMI-1 connected/'); then
    ~/.screenlayout/aid_desk.sh
fi

polybar -r mobile &

feh --bg-scale ~/Pictures/wallpapers/ds_static.jpg

xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mykbd $DISPLAY &

exec xmonad
