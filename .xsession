#!/bin/bash

echo `date +%F` >> ~/log/session.log

xrandr --fbmm 286x179
xrandr --dpi 192

setxkbmap -option "ctrl:swapcaps"

exec i3
