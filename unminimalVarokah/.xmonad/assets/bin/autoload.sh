#!/bin/sh

xmonad --restart;
xrdb ~/.Xresources &
xsetroot -cursor_name left_ptr &
nitrogen --restore &
mpd &
#volumeicon &
compton --config ~/.config/compton.conf -b &
