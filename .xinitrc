#!/usr/bin/env bash
xrdb -merge ~/.Xresources

PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

polybar -r default &

feh --bg-scale ~/Pictures/bg1.jpg

setxkbmap -option caps:escape &

exec ~/.cabal/bin/xmonad
