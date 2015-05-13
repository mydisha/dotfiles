#!/bin/sh
fg="#fcfcfc"
bg="#252525"
conky -c ~/.xmonad/scripts/dzenconky_1 | dzen2 -p -ta r -e 'button3=' -fn 'Exo 2-8' -fg "$fg" -bg "$bg" -h 20 -w 650 -x 720 -y 0