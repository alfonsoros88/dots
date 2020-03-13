#!/usr/bin/env bash
xrdb -merge ~/.Xresources

PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

/usr/lib/notification-daemon/notification-daemon &
polybar -r default &

feh --bg-scale ~/Pictures/bg5.jpg

setxkbmap -option caps:escape &

exec ~/.cabal/bin/xmonad
